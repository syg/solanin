module Solanin.Server (solanin,
                       serve) where

import Data.Char
import Data.Maybe
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map as M
import Control.Monad.Maybe
import Control.Monad.Reader
import System.Directory
import System.FilePath
import System.Process hiding (env)
import Network.Wai
import qualified Hyena.Server as Hyena
import Solanin.Server.Handler
import Solanin.Server.File
import Solanin.Server.StringTemplate
import Solanin.Server.Util
import Solanin.Song
import Solanin.Playlist (PlaylistEntry(..))
import qualified Solanin.Playlist as PL
import Solanin.State
import Solanin.Validate
import Paths_solanin

solanin :: Handler
solanin = do
  staticDir <- liftIO (getDataFileName "static")

  msum [prefix "/_s" (method Get (fileServer staticDir)),
        setup False (msum [path "/_c" configHandlers,
                           path "/_p" (method Post newPassword),
                           path "/_b" (method Post rebuildIndexH)]),

        setup False (return seeSetup),
        -- After this point we must be set up.
        path "/_l" loginHandlers,
        -- Invalid sessions are removed.
        authenticated' False (return seeLogin) removeSession,
        -- After this point we must be logged in.
        path "/_x" logout,
        path "/_c" configHandlers,
        path "/_p" passwdHandlers,
        path "/_b" rIndexHandlers,
        method Get renderPlayer,
        method Get transcodeServer]
  where
    seeSetup = seeOther "/_c"
    seeLogin = seeOther "/_l"
    removeSession = let
      cookie = cookieHeader (mkCookie (Just 0) ("sid", ""))
      in return (withHeaders [cookie] seeLogin)

    loginHandlers  = msum [method Get (renderLogin False),
                           method Post login]
    configHandlers = msum [method Get (renderConfig []),
                           method Post configure]
    passwdHandlers = msum [method Get (renderNewPassword []),
                           method Post newPassword]
    rIndexHandlers = msum [method Get renderRebuildIndex,
                           method Post rebuildIndexH]

renderConfig :: [(String, Validation (Maybe String))] -> Handler
renderConfig validations = do
  env    <- askEnvironment
  config <- askConfig >>= liftIO . readConfig
  renderST (if isXhr env then "configForm" else "config")
           [("config",      STB config),
            ("validations", STB (M.fromList validations))]

rebuildIndexH :: Handler
rebuildIndexH = do
  env     <- askEnvironment
  config  <- askConfig >>= liftIO . readConfig
  index   <- askIndex
  if null (configLibrary config) then
    return (serverError "cannot build index before configuring library")
    else do
      liftIO $ do
        rebuildIndex config index
        saveIndex index
      -- Refresh the default playlist.
      let Just sid = lookup "sid" (parseCookies' env)
      sd <- mkSessionData True ""
      askSessions >>= liftIO . (writeSession sid sd)
      if configSetup config then
        case isXhr env of
          True  -> do
            fp <- liftIO . canonicalizePath $ (configLibrary config)
            renderPlaylist "playlist" $ PL.lookup fp (sessionPlaylist sd)
          False -> return (seeOther "/")
        else renderNewPassword []

renderRebuildIndex :: Handler
renderRebuildIndex = do
  env    <- askEnvironment
  config <- askConfig >>= liftIO . readConfig
  if not (configSetup config) ||
     queryString env == Just (pack "confirmed") then
    renderST (if isXhr env then "rebuildIndexProgress" else "rebuildIndex")
             ([] :: [(String, String)])
    else renderST (if isXhr env then "rebuildIndexConfirmForm"
                     else "rebuildIndexConfirm")
                  ([] :: [(String, String)])

configure :: Handler
configure = do
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
      adjustConfig (\c -> c { configLibrary = library,
                              configIgnored = maybe [] commas ignored,
                              configFFmpeg  = ffmpeg,
                              configExts    = exts,
                              configBitrate = bitrate }) config
      saveConfig config
    renderRebuildIndex
    else renderConfig vs
  where
    -- If there is no FFmpeg binary available, or if the FFmpeg provided
    -- cannot encode mp3s, we can only support passthrough mp3s.
    supportedExts Nothing    = return [".mp3"]
    supportedExts (Just bin) = do
      formats <- ffmpegFormats bin
      case M.lookup "mp3" formats of
        Just cs -> if 'E' `elem` cs then
                     return (decodables songExts formats)
                     else return [".mp3"]
        Nothing -> return [".mp3"]

    decodables es fs = filter canDecode es
      where
        canDecode ext = case M.lookup (drop 1 ext) fs of
          Just cs -> 'D' `elem` cs
          Nothing -> False

    ffmpegFormats bin = do
      (_, out, _) <- readProcessWithExitCode bin ["-formats"] ""
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
      adjustConfig (\c -> c { configSetup = True,
                              configPassword = sha1Crypt salt new' }) config
      saveConfig config
    if configSetup config' then
      case isXhr env of
        True  -> return (ok (strHeaders "") (sendString ""))
        False -> return (seeOther "/")
      else do
        sd  <- mkSessionData True ""
        sid <- askSessions >>= liftIO . (writeNewSession sd)
        let cookie = cookieHeader (mkCookie Nothing ("sid", sid))
        return (withHeaders [cookie] (seeOther "/"))
    else renderNewPassword vs

mkSessionData :: Bool -> String -> HandlerT SessionData
mkSessionData b q = do
  lib   <- liftM configLibrary (askConfig >>= liftIO . readConfig)
  songs <- askIndex >>= liftIO . (queryIndex q)
  return $ SessionData
    { sessionLoggedIn = b
    , sessionPlaylist = PL.fromSet lib songs
    }

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
      True  -> return (ok nothingHeaders sendNothing)
      False -> do
        case lookup "sid" (parseCookies' env) of
          Just sid -> do
            let f s = s { sessionLoggedIn = True }
            askSessions >>= liftIO . (adjustSession sid f)
            return (seeOther "/")
          Nothing  -> do
            sd  <- mkSessionData True ""
            sid <- askSessions >>= liftIO . (writeNewSession sd)
            let cookie = cookieHeader (mkCookie Nothing ("sid", sid))
            return (withHeaders [cookie] (seeOther "/"))
    else case isXhr env of
      True  -> return unauthorized
      False -> renderLogin True

logout :: Handler
logout = do
  cookies <- liftM parseCookies' askEnvironment
  case lookup "sid" cookies of
    Just sid -> do
      let f s = s { sessionLoggedIn = False }
      askSessions >>= liftIO . (adjustSession sid f)
      return (seeOther "/")
    Nothing  -> return (seeOther "/")

renderPlaylist :: String -> [PlaylistEntry] -> Handler
renderPlaylist t pl = do
  lib <- liftM configLibrary (askConfig >>= liftIO . readConfig)
  let pl' = map (relTo lib) pl
  renderST t [("dirs",  filter PL.isDirectory pl'),
              ("songs", filter PL.isSong pl')]
  where
    relTo lib (DirEntry fp) =
      DirEntry (makeRelative lib fp)
    relTo lib (SongEntry s@Song { songPath = fp }) =
      SongEntry s { songPath = makeRelative lib fp }

renderPlayer :: Handler
renderPlayer = do
  env     <- askEnvironment
  config  <- askConfig >>= liftIO . readConfig
  Just sd <- currentSession

  fp <- liftIO . canonicalizePath $
        (configLibrary config) </>
        makeRelative "/" (unpack $ pathInfo env)
  case PL.lookup fp (sessionPlaylist sd) of
    [] -> mzero
    pl -> renderPlaylist (if isXhr env then "playlist" else "player") pl

serve :: Handler -> State -> IO ()
serve h state = Hyena.serve $ \env -> do
  r <- runMaybeT (runReaderT h (unescapeEnv env, state))
  case r of
    Just h' -> return h'
    Nothing -> return notFound
  where
    unescapeEnv env = env
      { pathInfo    = (pack . unescape . unpack) (pathInfo env)
      , queryString = liftM (pack . unescape . unpack) (queryString env)
      }
