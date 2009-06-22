module Solanin.Data (PlaylistEntry(..),

                     isDirectory,
                     isSong,
                     directories,
                     songs,
                     open,
                     exts) where

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Sound.TagLib as TagLib
import qualified Data.Map as M
import System.Directory
import System.FilePath

data PlaylistEntry = Song
  { path     :: FilePath
  , track    :: Integer
  , title    :: String
  , artist   :: String
  , album    :: String
  , duration :: Integer
  }                | Directory String
  deriving (Show, Eq)

exts = [".mp3", ".flac", ".ape", ".tta", ".wav"]

-- | Opens a file/directory and possibly returns a PlaylistEntry.
-- Nothing is returned if:
--   * There is an error with opening the file.
--   * The file does not have one of the recognized extensions.
open :: [String] -> FilePath -> IO (Maybe PlaylistEntry)
open exts f = catch (open' exts f) (const (return Nothing))

open' exts f = do
  -- TagLib.open doesn't throw exceptions, so we have to check ourselves if we
  -- can read the file
  perms <- getPermissions f
  if readable perms then open'' exts f else return Nothing

open'' exts f = do
  fe <- doesDirectoryExist f
  case fe of
    True  -> return $ Just (Directory f)
    False -> if takeExtension f `elem` exts then do
      tf <- TagLib.open (UTF8.decodeString f)
      duration <- tagDuration tf
      let r = return $ Just (Song f 0 (takeBaseName f) "" "" duration)
      maybe r (g r duration) tf
      else return Nothing
  where
    tagDuration (Just tf) = do
      p <- TagLib.audioProperties tf
      case p of
        Just p  -> TagLib.duration p
        Nothing -> return 0
    tagDuration Nothing = return 0

    g r duration tf = do
      t <- TagLib.tag tf
      maybe r (g' tf duration) t
    g' tf duration t = do
      p      <- TagLib.audioProperties tf
      track  <- TagLib.track t
      title  <- TagLib.title t
      artist <- TagLib.artist t
      album  <- TagLib.album t

      return $ Just (Song f
                          track
                          (UTF8.encodeString title)
                          (UTF8.encodeString artist)
                          (UTF8.encodeString album)
                          duration)

isDirectory :: PlaylistEntry -> Bool
isDirectory (Directory _) = True
isDirectory _ = False

isSong :: PlaylistEntry -> Bool
isSong (Song _ _ _ _ _ _) = True
isSong _ = False

directories :: [PlaylistEntry] -> [PlaylistEntry]
directories = filter isDirectory

songs :: [PlaylistEntry] -> [PlaylistEntry]
songs = filter isSong
