module Solanin.Server (solanin,
                       serve) where

import Data.Char
import Data.Monoid
import Data.List (sortBy, (\\))
import Data.Maybe
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map as M
import Control.Concurrent.STM
import Control.Monad.Maybe
import Control.Monad.Reader
import System.Directory
import System.FilePath
import System.Process
import Network.URI (unEscapeString)
import Network.Wai
import qualified Hyena.Server as Hyena
import qualified Solanin.Data as D
import Solanin.Server.Handler
import Solanin.Server.File
import Solanin.Server.StringTemplate
import Solanin.Server.Util
import Solanin.State
import Solanin.Validate

solanin :: Handler
solanin =
  msum [prefix "/_s" (method Get (fileServer "static")),
        setup False (msum [path "/_c" configHandlers,
                           path "/_p" (method Post newPassword)]),

        setup False (return seeSetup),
        -- after this point we must be set up
        path "/_l" loginHandlers,
        -- invalid sessions are removed
        --authenticated' False (return seeLogin) removeSession,
        -- after this point we must be logged in
        path "/_l" logout,
        path "/_c" configHandlers,
        path "/_p" passwdHandlers,
        method Get renderPlayer,
        method Get transcodeServer]
  where
    seeSetup = seeOther "/_c"
    seeLogin = seeOther "/_l"
    {-
    removeSession = do
      let cookie = cookieHeader (mkCookie 0 ("sid", ""))
      return (withHeaders [cookie] seeLogin)
    -}

    loginHandlers  = msum [method Get (renderLogin False),
                           method Post login]
    configHandlers = msum [method Get (renderConfig []),
                           method Post config]
    passwdHandlers = msum [method Get (renderNewPassword []),
                           method Post newPassword]

renderConfig :: [(String, Validation (Maybe String))] -> Handler
renderConfig validations = do
  env    <- askEnvironment
  config <- askConfig >>= liftIO . readConfig
  renderST (if isXhr env then "configForm" else "config")
           [("config",      STB config),
            ("validations", STB (M.fromList validations))]

config :: Handler
config = do
  env  <- askEnvironment
  form <- liftIO (decodeForm env)
  vs   <- validate form [("library", [vNotBlank, vLibrary]),
                         ("ignored", [vAny]),
                         ("ffmpeg",  [vFFmpeg]),
                         ("bitrate", [vNumber])]
  if null (invalids vs) then do
    let vs'     = valids vs
        library = fromJust2 (lookup "library" vs')
        ignored = fromJust (lookup "ignored" vs')
        ffmpeg  = fromJust (lookup "ffmpeg" vs')
        bitrate = (read . fromJust2) (lookup "bitrate" vs')
    config <- askConfig
    liftIO $ do
      exts <- supportedExts ffmpeg
      updateConfig (\c -> c { configLibrary = library,
                              configIgnored = maybe [] commas ignored,
                              configFFmpeg  = ffmpeg,
                              configExts    = exts,
                              configBitrate = bitrate }) config
      saveConfig config
    config' <- liftIO (readConfig config)
    if configSetup config' then
      case isXhr env of
        -- TODO better way of communicating success without something as
        --      all-out as JSON?
        True  -> return (ok (strHeaders "") (sendString ""))
        False -> return (seeOther "/")
      else renderNewPassword []
    else renderConfig vs
  where
    -- if there is no FFmpeg binary available, or if the FFmpeg provAnyed
    -- cannot encode mp3s, we can only support passthrough mp3s
    supportedExts Nothing    = return [".mp3"]
    supportedExts (Just bin) = do
      formats <- ffmpegFormats bin
      case M.lookup "mp3" formats of
        Just cs -> if 'E' `elem` cs then
                     return (decodables D.exts formats)
                     else return [".mp3"]
        Nothing -> return [".mp3"]

    decodables es fs = filter (canDecode fs) es
      where
        canDecode fs ext = case M.lookup (drop 1 ext) fs of
          Just cs -> 'D' `elem` cs
          Nothing -> False

    ffmpegFormats bin = do
      (ec, out, _) <- readProcessWithExitCode bin ["-formats"] ""
      return $ M.fromList (formats (dropWhile (== "File formats:")
                                   (takeWhile (/= "") (lines $ fixCR out))))
      where
        formats [] = []
        formats (l:ls) = (head (words (drop 4 l)), take 3 l) : formats ls

        fixCR :: String -> String
        fixCR [] = []
        fixCR ('\r':'\n':s) = '\n' : fixCR s
        fixCR ('\r':s) = '\n' : fixCR s
        fixCR (c:s) = c : fixCR s

    fromJust2 = fromJust . fromJust

    commas s =  case dropWhile (== ',') s of
      "" -> []
      s' -> dropWhile isSpace w : commas s''
            where
              (w, s'') = break (== ',') s'

renderNewPassword :: [(String, Validation (Maybe String))] -> Handler
renderNewPassword validations = do
  env    <- askEnvironment
  config <- askConfig >>= liftIO . readConfig
  renderST (if isXhr env then "passwordForm" else "password")
           [("config",      STB config),
            ("validations", STB (M.fromList validations))]

newPassword :: Handler
newPassword = do
  env     <- askEnvironment
  form    <- liftIO (decodeForm env)
  config  <- askConfig
  config' <- liftIO (readConfig config)
  let new   = lookup "newPassword" form
      crypt = configPassword config'
  vs <- if configSetup config' then
          validate form [("oldPassword",     [vPassword crypt]),
                         ("confirmPassword", [vEqual new])]
          else validate form [("confirmPassword", [vEqual new])]
  if null (invalids vs) then do
    liftIO $ do
      let new' = maybe "" id new
      salt <- mkSalt 8
      updateConfig (\c -> c { configSetup = True,
                              configPassword = sha1Crypt salt new' }) config
      saveConfig config
    if configSetup config' then
      case isXhr env of
        True  -> return (ok (strHeaders "") (sendString ""))
        False -> return (seeOther "/")
      else do
        sid <- askSessions >>= liftIO . (writeNewSession True)
        let cookie = cookieHeader (mkCookie (-1) ("sid", sid))
        return (withHeaders [cookie] (seeOther "/"))
    else renderNewPassword vs

renderLogin :: Bool -> Handler
renderLogin True  = do
  response <- renderST "login" [("failed", True)]
  return (withStatus (401, pack "Unauthorized") response)
renderLogin False = renderST "login" [("failed", False)]

login :: Handler
login = do
  env   <- askEnvironment
  crypt <- liftM configPassword (askConfig >>= liftIO . readConfig)
  form  <- liftIO (decodeForm env)
  vs    <- validate form [("password", [vPassword crypt])]
  if null (invalids vs) then do
    case isXhr env of
      True  -> return (ok (strHeaders "") (sendString ""))
      False -> do
        case lookup "sid" (parseCookies' env) of
          Just sid -> do
            askSessions >>= liftIO . (writeSession sid True)
            return (seeOther "/")
          Nothing  -> do
            sid <- askSessions >>= liftIO . (writeNewSession True)
            let cookie = cookieHeader (mkCookie (-1) ("sid", sid))
            return (withHeaders [cookie] (seeOther "/"))
    else case isXhr env of
      True  -> return unauthorized
      False -> renderLogin True

logout :: Handler
logout = do
  cookies <- liftM parseCookies' askEnvironment
  case lookup "sid" cookies of
    Just sid -> do
      askSessions >>= liftIO . (writeSession sid False)
      return (seeOther "/")
    Nothing  -> return (seeOther "/")

playlist :: FilePath -> [FilePath] -> [String] -> IO [D.PlaylistEntry]
playlist d ignored exts = do
  fs <- getDirectoryContents d
  pl <- mapM open (map (d </>) (fs \\ (ignored ++ [".", ".."])))
  return $ catMaybes pl
  where
    open = D.open exts

renderPlaylist :: String -> [D.PlaylistEntry] -> Handler
renderPlaylist t pl = do
  lib <- liftM configLibrary (askConfig >>= liftIO . readConfig)
  let pl' = map (relTo lib) pl
  renderST t [("dirs",  D.directories pl'),
              ("songs", sortBy cmp (D.songs pl'))]
  where
    relTo lib (D.Directory f) = D.Directory (makeRelative lib f)
    relTo lib s@(D.Song f _ _ _ _ _) = s { D.path = makeRelative lib f }

    -- songs are sorted by album then track number
    cmp a b | D.album a == D.album b = D.track a `compare` D.track b
            | otherwise              = D.album a `compare` D.album b

renderPlayer :: Handler
renderPlayer = do
  env    <- askEnvironment
  config <- askConfig >>= liftIO . readConfig
  let lib     = configLibrary config
      ignored = configIgnored config
      exts    = configExts config
      fp      = lib </> makeRelative "/" (unpack $ pathInfo env)
  d <- liftIO (doesDirectoryExist fp)
  if d then do
    pl <- liftIO $ catch (playlist fp ignored exts) (const (return []))
    renderPlaylist (if isXhr env then "playlist" else "player") pl
    else mzero

serve :: Handler -> State -> IO ()
serve h state = Hyena.serve $ \env -> do
  r <- runMaybeT (runReaderT h (unescapeEnv env, state))
  case r of
    Just h' -> return h'
    Nothing -> return notFound
  where
    unescape = pack . unEscapeString . unpack
    unescapeEnv env = env
      { pathInfo    = unescape (pathInfo env),
        queryString = liftM unescape (queryString env) }
