module Solanin.State where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import Data.Word
import Data.Digest.Pure.SHA
import Text.JSON
import Control.Monad (liftM, replicateM, unless)
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import System.Random

type State    = (TVar Config, TVar Sessions)
type Sessions = M.Map String Bool

data Config = Config
  { configSetup    :: Bool
  , configLibrary  :: FilePath
  , configIgnored  :: [String]
  , configFFmpeg   :: Maybe FilePath
  , configExts     :: [String]
  , configBitrate  :: Int
  , configPassword :: String
  } deriving (Show, Eq)

instance JSON Config where
  readJSON (JSObject obj) = do
    setup    <- lookup' "setup" configSetup
    library  <- lookup' "library" configLibrary
    ignored  <- lookup' "ignored" configIgnored
    ffmpeg   <- lookup' "ffmpeg" configFFmpeg
    exts     <- lookup' "exts" configExts
    bitrate  <- lookup' "bitrate" configBitrate
    password <- lookup' "password" configPassword
    return $ Config setup library ignored ffmpeg exts bitrate password
    where
      assocs = fromJSObject obj
      lookup' k f = maybe (Error "malformed config")
                          readJSON
                          (lookup k assocs)
  readJSON _ = Error "malformed config"

  showJSON c = makeObj assocs
    where
      assocs = [("setup",    showJSON $ configSetup c),
                ("library",  showJSON $ configLibrary c),
                ("ignored",  showJSON $ configIgnored c),
                ("ffmpeg",   showJSON $ configFFmpeg c),
                ("exts",     showJSON $ configExts c),
                ("bitrate",  showJSON $ configBitrate c),
                ("password", showJSON $ configPassword c)]

nullConfig = Config
  { configSetup    = False
  , configLibrary  = ""
  , configIgnored  = []
  , configFFmpeg   = Nothing
  , configExts     = []
  , configBitrate  = 128
  , configPassword = ""
  }

configFile = "solanin.conf"

newSessions :: IO (TVar Sessions)
newSessions = atomically $ newTVar M.empty

readSession :: String -> TVar Sessions -> IO (Maybe Bool)
readSession k s = (atomically . readTVar) s >>= return . (M.lookup k)

writeSession :: String -> Bool -> TVar Sessions -> IO ()
writeSession k v s = atomically $ readTVar s >>= writeTVar s . (M.insert k v)

writeNewSession :: Bool -> TVar Sessions -> IO String
writeNewSession v s = do
  sid     <- liftM (sha1 . L.pack) (replicateM 8 (getStdRandom random))
  written <- atomically $ writeUnique (show sid)
  if written then return (show sid) else writeNewSession v s
  where
    writeUnique sid = do
      s' <- readTVar s
      case M.lookup sid s' of
        Just _  -> return False
        Nothing -> writeTVar s (M.insert sid v s') >> return True

newConfig :: IO (TVar Config)
newConfig = atomically $ newTVar nullConfig

readConfig :: TVar Config -> IO Config
readConfig = atomically . readTVar

writeConfig :: Config -> TVar Config -> IO ()
writeConfig c x = atomically $ writeTVar x c

updateConfig :: (Config -> Config) -> TVar Config -> IO ()
updateConfig f x = atomically $ readTVar x >>= (writeTVar x) . f

loadConfig :: IO (TVar Config)
loadConfig = do
  result <- liftM decode (readFile configFile)
  case result of
    Ok c    -> atomically $ newTVar c
    Error _ -> fail "malformed config"

saveConfig :: TVar Config -> IO ()
saveConfig c = readConfig c >>= writeFile configFile . encode

sha1Crypt :: String -> String -> String
sha1Crypt salt plain = salt' ++ (show $ (sha1 . L.pack) (salt' ++ plain))
  where
    salt' = takeWhile (/= '$') salt ++ "$"

mkSalt :: Int -> IO String
mkSalt n = replicateM n (randomIdx >>= return . (hexL !!))
  where
    hexL = ['0'..'9'] ++ ['a'..'f']
    randomIdx = getStdRandom (randomR (0, length hexL - 1))
