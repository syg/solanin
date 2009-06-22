{-# LANGUAGE ImpredicativeTypes #-}

module Solanin.Server.File (fileServer,
                            transcodeServer,
                            mimetypeOf) where

import qualified Data.ByteString as S
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Control.Exception (evaluate)
import Control.Concurrent (forkIO)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import System.FilePath
import System.Directory
import System.Process
import System.IO
import System.Locale (defaultTimeLocale)
import System.Time
import Network.Wai
import Solanin.Server.Handler
import Solanin.Server.Util
import Solanin.State

type Processor = FilePath -> IO (Int, S.ByteString, Headers, Enumerator)

sendHandle :: (Handle -> Int -> IO S.ByteString)
           -> (Handle -> IO ())
           -> Handle
           -> Enumerator
sendHandle get close h f z = do
  block <- get h blockSize
  if S.null block then close h >> return z
    else do
      z' <- f z block
      case z' of
        Left z''  -> close h >> return z''
        Right z'' -> sendHandle get close h f z''

fileServer :: String -> Handler
fileServer root = do
  env <- askEnvironment
  liftIO (serve root env fileProcessor)

fileProcessor :: Processor
fileProcessor fp = do
  h  <- openBinaryFile fp ReadMode
  sz <- hFileSize h
  let headers = [("Content-Type", mimetypeOf fp),
                 ("Content-Length", show sz)]
  return (ok (packHeaders headers)
             (sendHandle S.hGetNonBlocking hClose h))

transcodeServer :: Handler
transcodeServer = do
  env    <- askEnvironment
  config <- askConfig >>= liftIO . readConfig
  let root = configLibrary config
  if queryString env == Just (pack "tr") &&
     isJust (configFFmpeg config) then
    liftIO (serve root env (transcodeProcessor config))
    else fileServer root

transcodeProcessor :: Config -> Processor
transcodeProcessor config fp =
  if (takeExtension fp) `elem` (configExts config) then do
    (_, Just outh, Just errh, ph) <- createProcess ffmpeg
    hClose errh
    let headers = [("Content-Type", mimetypeOf ".mp3"),
                   ("Connection", "close")]
    return (ok (packHeaders headers)
               (sendHandle S.hGet (close ph) outh))
    else return (serverError "cannot transcode")
  where
    close ph h = hClose h >> waitForProcess ph >> return ()

    ffbin  = fromJust (configFFmpeg config)
    ffargs = ["-i", fp, "-ab", show (configBitrate config) ++ "k",
              "-f", "mp3", "-"]
    ffmpeg = (proc ffbin ffargs) { std_out = CreatePipe,
                                   std_err = CreatePipe }

serve :: FilePath
      -> Environment
      -> (FilePath -> IO (Int, S.ByteString, Headers, Enumerator))
      -> IO (Int, S.ByteString, Headers, Enumerator)
serve root env p = do
  let fp = root </> makeRelative "/" (unpack $ pathInfo env)
  r <- catch (liftM readable $ getPermissions fp) (const (return False))
  d <- doesDirectoryExist fp
  if r && not d then do
    lmtime <- getModificationTime fp
    let mtime = formatCalendarTime defaultTimeLocale
                "%a, %d %b %Y %X GMT" (toUTCTime lmtime)
    if notModifiedSince mtime env then return notModified
      else let
        headers = packHeaders [("Last-Modified", mtime)]
        in liftM (withHeaders headers) (p fp)
    else return notFound
  where
    notModifiedSince mtime env =
      lookup (pack "If-Modified-Since") (headers env) == Just (pack mtime)

mimetypes = M.fromList
  [(".css", "text/css"),
   (".txt", "text/plain"),
   (".html", "text/html"),
   (".mp3", "audio/mpeg"),
   (".flac", "audio/flac"),
   (".ape", "audio/x-ape"),
   (".wav", "audio/x-wav"),
   (".jpg", "image/jpeg"),
   (".png", "image/png")]

mimetypeOf :: FilePath -> String
mimetypeOf fp = M.findWithDefault defaultType (takeExtension fp) mimetypes
  where
    defaultType = "application/octet-stream"
