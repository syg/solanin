module Solanin.Playlist where

import qualified Data.ByteString.UTF8 as U
import qualified Data.Trie as T
import qualified Data.Trie.Convenience as T
import qualified Data.Set as S
import System.FilePath
import Solanin.Song
import Solanin.Server.Util (normalizeString)

data PlaylistEntry = SongEntry Song
                   | DirEntry FilePath
  deriving (Show, Eq)

type Playlist = T.Trie [PlaylistEntry]

instance Ord PlaylistEntry where
  compare (SongEntry x) (SongEntry y) = x `compare` y
  compare (DirEntry  _) (SongEntry _) = LT
  compare (SongEntry _) (DirEntry  _) = GT
  compare (DirEntry  x) (DirEntry  y) = n x `compare` n y
    where
      n = normalizeString

fromSet :: FilePath -> S.Set Song -> Playlist
fromSet root ss = fmap S.toAscList (S.fold f T.empty ss)
  where
    loop d v t | d == root = T.insertWith (S.union) (U.fromString d) v t
               | otherwise = loop (takeDirectory d) v' $
                             T.insertWith (S.union) (U.fromString d) v t
      where
        v' = S.singleton (DirEntry d)

    f s t = loop (takeDirectory k) v t
      where
        k = songPath s
        v = S.singleton (SongEntry s)

lookup :: FilePath -> Playlist -> [PlaylistEntry]
lookup fp pl = T.lookupWithDefault [] (U.fromString fp) pl

isDirectory :: PlaylistEntry -> Bool
isDirectory (DirEntry _) = True
isDirectory _ = False

isSong :: PlaylistEntry -> Bool
isSong (SongEntry _) = True
isSong _ = False
