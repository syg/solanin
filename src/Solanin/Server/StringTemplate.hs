{-# LANGUAGE ExistentialQuantification, ImpredicativeTypes #-}

module Solanin.Server.StringTemplate (STBox(..),

                                      renderST,
                                      renderST') where

import qualified Data.ByteString as S (ByteString, length)
import Data.List (intercalate)
import qualified Data.Map as M
import Control.Monad.Trans (MonadIO(..))
import Text.StringTemplate
import Text.StringTemplate.Classes (ToSElem(..), SElem(..))
import System.FilePath
import Network.Wai
import qualified Network.URI as URI
import Solanin.Song (Song(..))
import Solanin.Playlist (PlaylistEntry(..))
import Solanin.Server.Util
import Solanin.State
import Solanin.Validate
import Paths_solanin

data STBox = forall a. ToSElem a => STB a

instance ToSElem STBox where
  toSElem (STB a) = toSElem a

instance ToSElem Config where
  toSElem (Config setup lib ig ffmpeg exts bitrate _) =
    SM $ M.fromList [("setup", toSElem setup),
                     ("library", STR lib),
                     ("ignored", STR $ intercalate ", " ig),
                     ("ffmpeg", toSElem ffmpeg),
                     ("exts", toSElem exts),
                     ("bitrate", toSElem bitrate)]

instance ToSElem Song where
  toSElem (Song path track title artist album duration) = 
    SM $ M.fromList [("song", STR $ escapeFileURL path),
                     ("track", STR $ trackToString track),
                     ("title", if null title then
                                 STR (takeFileName path)
                                 else STR title),
                     ("artist", STR artist),
                     ("album", STR album),
                     ("duration", STR $ formatDuration duration)]
    where
      trackToString t | t == 0    = ""
                      | t < 10    = "0" ++ show t
                      | otherwise = show t

      formatDuration s | s == 0 = ""
                       | s > 60*60 = let
        h = s `div` 60*60
        in show h ++ ":" ++ formatDuration (s - h*60*60)
                       | otherwise = let
        m = s `div` 60
        s' = s - m*60
        in show m ++ ":" ++ (if s' < 10 then "0" else "") ++ show s'

instance ToSElem PlaylistEntry where
  toSElem (SongEntry s) = toSElem s
  toSElem (DirEntry  d) =
    SM $ M.fromList [("directory", STR $ escapeFileURL d),
                     ("basename", STR $ takeFileName d ++ "/")]

instance (ToSElem a) => ToSElem (Validation a) where
  toSElem (Valid a)   = SM $ M.fromList [("valid", toSElem a)]
  toSElem (Invalid x) = SM $ M.fromList [("invalid", STR x)]
  toSElem _           = SNull

renderST' :: (ToSElem a, MonadIO m)
          => FilePath
          -> String
          -> [(String, a)]
          -> m (Int, S.ByteString, Headers, Enumerator)
renderST' root name attrs = do
  g <- liftIO $ directoryGroupLazy root
  case getStringTemplate name g of
    Just t  -> let
      s  = render $ setManyAttrib attrs t
      hs = [("Content-Type", "text/html; charset=utf-8"),
            ("Content-Length", show (S.length s))]
      in return (ok (packHeaders hs) (sendBytes s))
    Nothing -> return notFound

renderST :: (ToSElem a, MonadIO m)
         => String
         -> [(String, a)]
         -> m (Int, S.ByteString, Headers, Enumerator)
renderST name attrs = do
  d <- liftIO (getDataFileName "templates")
  renderST' d name attrs

escapeFileURL :: FilePath -> FilePath
escapeFileURL f = intercalate "/" $
                  map (URI.escapeURIString URI.isUnreserved)
                      (splitDirectories f)
