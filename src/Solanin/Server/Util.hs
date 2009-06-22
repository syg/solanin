{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}

module Solanin.Server.Util where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Either
import Data.Maybe (catMaybes)
import Data.List (intercalate, lookup)
import Text.JSON (JSON, encode)
import Text.ParserCombinators.Parsec
import Network.URI (unEscapeString)
import Network.Wai

data Cookie = Cookie
  { cookieVersion :: String
  , cookieName    :: String
  , cookieValue   :: String
  , cookieMaxAge  :: Int
  , cookiePath    :: Maybe FilePath
  , cookieDomain  :: Maybe String
  } deriving (Show, Eq)

blockSize :: Int
blockSize = 4 * 1024

withHeaders :: [(ByteString, ByteString)]
            -> (Int, ByteString, Headers, Enumerator)
            -> (Int, ByteString, Headers, Enumerator)
withHeaders h (rcode, rmsg, h', enum) = (rcode, rmsg, h' ++ h, enum)

withStatus :: (Int, ByteString)
            -> (Int, ByteString, Headers, Enumerator)
            -> (Int, ByteString, Headers, Enumerator)
withStatus (rcode, rmsg) (_, _, h, enum) = (rcode, rmsg, h, enum)

packHeaders :: [(String, String)] -> [(ByteString, ByteString)]
packHeaders hs = [ (C.pack a, C.pack b) | (a, b) <- hs ]

strHeaders :: String -> [(ByteString, ByteString)]
strHeaders s = packHeaders hs
  where
    hs = [("Content-Type", "text/plain"),
          ("Content-Length", show (C.length $ C.pack s))]

sendString :: String -> Enumerator
sendString s = sendBytes (C.pack s)

sendBytes :: ByteString -> Enumerator
sendBytes s f z = let
  block = C.take blockSize s
  in if C.null block then return z
    else do
      z' <- f z block
      case z' of
        Left z''  -> return z''
        Right z'' -> sendBytes (C.drop blockSize s) f z''

ok :: Headers -> Enumerator -> (Int, ByteString, Headers, Enumerator)
ok h e = (200, C.pack "OK", h, e)

unauthorized :: (Int, ByteString, Headers, Enumerator)
unauthorized = (401,
                C.pack "Unauthorized",
                strHeaders msg,
                sendString msg)
  where
    msg = "401 - Unauthorized"

notFound :: (Int, ByteString, Headers, Enumerator)
notFound = (404,
            C.pack "Not Found",
            strHeaders msg,
            sendString msg)
  where
    msg = "404 - Not Found"

seeOther :: String -> (Int, ByteString, Headers, Enumerator)
seeOther l = (303,
              C.pack "See Other",
              packHeaders [("Content-Length", "0"),
                           ("Location", l)],
              sendString "")

notModified :: (Int, ByteString, Headers, Enumerator)
notModified = (304,
               C.pack "Not Modified",
               strHeaders "",
               sendString "")

serverError :: String -> (Int, ByteString, Headers, Enumerator)
serverError msg = (500,
                   C.pack "Internal Server Error",
                   strHeaders msg,
                   sendString msg)

mkCookie :: Int -> (String, String) -> Cookie
mkCookie a (k, v) = Cookie "1" k v a (Just "/") Nothing

cookieHeader :: Cookie -> (ByteString, ByteString)
cookieHeader c = (C.pack "Set-Cookie", C.pack $ intercalate "; " props)
  where
    age = cookieMaxAge c

    val    = pair (cookieName c) (cookieValue c)
    maxAge = pair "Max-Age" (if age < 0 then "" else show age)
    ver    = pair "Version" (cookieVersion c)
    path   = fmap (pair "Path") (cookiePath c)
    domain = fmap (pair "Domain") (cookieDomain c)
    props  = [val, maxAge, ver] ++ catMaybes [path, domain]

    pair k v = k ++ "=\"" ++ v ++ "\""

parseCookies :: Environment -> [Cookie]
parseCookies env =
  case lookup (C.pack "Cookie") (headers env) of
    Just v  -> either eh catMaybes (parseCookies v)
      where
        eh e = error ("Server.Util.parseCookies: " ++ show e)
    Nothing -> []
  where

    parseCookies = (parse cookies "") . C.unpack

    cookies = do
      ver <- option "0" (do { v <- reserved "$Version"; semi; return v })
      sepBy1 (cookie ver) (comma <|> semi)

    cookie ver = do
      (k, v) <- do { k <- name; eq; v <- value; return (k, v) }
      p <- optionMaybe (try (semi >> reserved "$Path"))
      d <- optionMaybe (try (semi >> reserved "$Domain"))
      -- XXX doesn't check Domain
      return $ case p of
        Nothing -> Just (Cookie ver k v 0 p d)
        Just p' -> if (C.pack p') `C.isPrefixOf` (pathInfo env) then
                     Just (Cookie ver k v 0 p d)
                     else Nothing

    reserved r = string r >> eq >> value >>= return
    comma      = between spaces space (char ',')
    semi       = between spaces space (char ';')
    name       = many1 alphaNum
    value      = between (char '"') (char '"') $ many1 alphaNum
    eq         = between spaces spaces $ char '='

parseCookies' :: Environment -> [(String, String)]
parseCookies' env = map f (parseCookies env)
  where
    f (Cookie _ k v _ _ _) = (k, v)

decodeForm :: Environment -> IO [(String, String)]
decodeForm env =
  case lookup (C.pack "Content-Type") (headers env) of
    Just t | t == (C.pack "application/x-www-form-urlencoded") -> do
      parsed <- input env pairs (C.empty, [])
      case parsed of
        (bs, ps) | C.length bs == 0 -> return ps
                 | otherwise        -> error "malformed input\n"
    _ -> error "Server.Util.decodeForm: input Content-Type unsupported\n"
  where
    pairs (partial, seed) bs = let
      sp  = C.split '&' (C.append partial bs)
      ps  = map pair sp
      ps' = reverse $ tail (reverse ps)
      in if null (lefts ps) then
        return $ Right (C.empty, seed ++ rights ps)
        else if null (lefts ps') then
          -- if only the last element failed to parse, maybe
          -- more data's coming
          return $ Right (last sp, seed ++ rights ps')
          else return $ Left (bs, [])

    pair bs = case C.split '=' bs of
      [k, v] -> Right (unescape k, unescape v)
      _      -> Left "malformed input"

    unescape = unEscapeString . C.unpack

isXhr :: Environment -> Bool
isXhr env =
  case lookup (C.pack "X-Requested-With") (headers env) of
    Just h  -> h == C.pack "XMLHttpRequest"
    Nothing -> False
