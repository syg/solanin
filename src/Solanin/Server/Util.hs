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
  , cookieMaxAge  :: Maybe Int
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

nothingHeaders :: [(ByteString, ByteString)]
nothingHeaders = packHeaders [("Content-Length", "0")]

strHeaders :: String -> [(ByteString, ByteString)]
strHeaders s = packHeaders hs
  where
    hs = [("Content-Type",   "text/plain"),
          ("Content-Length", show (C.length $ C.pack s))]

sendNothing :: Enumerator
sendNothing = const return

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
              nothingHeaders ++ (packHeaders [("Location", l)]),
              sendNothing)

notModified :: (Int, ByteString, Headers, Enumerator)
notModified = (304,
               C.pack "Not Modified",
               nothingHeaders,
               sendNothing)

serverError :: String -> (Int, ByteString, Headers, Enumerator)
serverError msg = (500,
                   C.pack "Internal Server Error",
                   strHeaders msg,
                   sendString msg)

mkCookie :: Maybe Int -> (String, String) -> Cookie
mkCookie a (k, v) = Cookie "1" k v a (Just "/") Nothing

cookieHeader :: Cookie -> (ByteString, ByteString)
cookieHeader c = (C.pack "Set-Cookie", C.pack $ intercalate "; " props)
  where
    val    = pair (cookieName c) (quote (cookieValue c))
    maxAge = fmap (pair "Max-Age") (fmap show (cookieMaxAge c))
    ver    = pair "Version" (quote (cookieVersion c))
    path   = fmap (pair "Path") (cookiePath c)
    domain = fmap (pair "Domain") (fmap quote (cookieDomain c))
    props  = [val, ver] ++ catMaybes [maxAge, path, domain]

    quote s  = "\"" ++ s ++ "\""
    pair k v = k ++ "=" ++ v

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
        Nothing -> Just (Cookie ver k v Nothing p d)
        Just p' -> if (C.pack p') `C.isPrefixOf` (pathInfo env) then
                     Just (Cookie ver k v Nothing p d)
                     else Nothing

    reserved r = string r >> eq >> value >>= return
    comma      = between spaces space (char ',')
    semi       = between spaces space (char ';')
    name       = many1 alphaNum
    quote      = option '"' (char '"')
    value      = between quote quote $ many1 alphaNum
    eq         = between spaces spaces $ char '='

parseCookies' :: Environment -> [(String, String)]
parseCookies' env = map f (parseCookies env)
  where
    f (Cookie _ k v _ _ _) = (k, v)

decodeForm :: Environment -> IO [(String, String)]
decodeForm env =
  case lookup (C.pack "Content-Type") (headers env) of
    Just t | urlencoded `C.isPrefixOf` t -> decode env
           | otherwise -> e ("unsupported Content-Type: " ++
                              C.unpack t ++ "\n")
    Nothing -> error ("no Content-Type\n")
  where
    e msg = error ("Server.Util.decodeForm: " ++ msg)
    urlencoded = C.pack "application/x-www-form-urlencoded"

    decode env = do
      parsed <- input env pairs (C.empty, [])
      case parsed of
        (bs, ps) | C.length bs == 0 -> return ps
                 | otherwise        -> error "malformed input\n"

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
      [k, v] -> Right (unescape (C.unpack k), unescape (C.unpack v))
      _      -> Left "malformed input"

unescape :: String -> String
unescape = unplus . unEscapeString
  where
    unplus []      = ""
    unplus ('+':s) = ' ' : unplus s
    unplus (  c:s) =   c : unplus s

isXhr :: Environment -> Bool
isXhr env =
  case lookup (C.pack "X-Requested-With") (headers env) of
    Just h  -> h == C.pack "XMLHttpRequest"
    Nothing -> False
