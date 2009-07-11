module Solanin.State (State(..),

                      SessionData(..),
                      Sessions,
                      newSessions,
                      readSession,
                      writeSession,
                      adjustSession,
                      writeNewSession,

                      Config(..),
                      emptyConfig,
                      newConfig,
                      readConfig,
                      writeConfig,
                      adjustConfig,
                      loadConfig,
                      saveConfig,
                      sha1Crypt,
                      mkSalt,

                      Index(..),
                      emptyIndex,
                      rebuildIndex,
                      queryIndex,
                      newIndex,
                      readIndex,
                      saveIndex,
                      loadIndex) where

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Trie as T
import qualified Data.Trie.Convenience as T
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Binary as B
import Data.Foldable (foldrM)
import Data.Monoid
import Data.Char (isSpace)
import Data.List ((\\))
import Data.Maybe (catMaybes, fromJust)
import Data.Digest.Pure.SHA
import Codec.Compression.GZip
import Text.JSON
import Text.ParserCombinators.Parsec hiding (State)
import Control.Exception (throw, evaluate)
import Control.Monad (liftM, liftM2, replicateM, filterM)
import Control.Concurrent.STM
import System.IO
import System.Random
import System.Directory
import System.FilePath ((</>), takeDirectory)
import System.Time
import Solanin.Song
import Solanin.Playlist hiding (lookup)
import Solanin.Server.Util (normalizeString)

data State = State
  { stateConfig   :: TVar Config
  , stateSessions :: TVar Sessions
  , stateIndex    :: TVar Index
  }

--
-- Sessions
--

data SessionData = SessionData
  { sessionLoggedIn :: Bool
  , sessionPlaylist :: Playlist
  } deriving (Show, Eq)

type Sessions = M.Map String SessionData

newSessions :: IO (TVar Sessions)
newSessions = atomically $ newTVar M.empty

readSession :: String -> TVar Sessions -> IO (Maybe SessionData)
readSession k s = (atomically . readTVar) s >>= return . (M.lookup k)

writeSession :: String
             -> SessionData
             -> TVar Sessions
             -> IO ()
writeSession k v s = atomically $
  readTVar s >>= writeTVar s . (M.insert k v)

adjustSession :: String
              -> (SessionData -> SessionData)
              -> TVar Sessions
              -> IO ()
adjustSession k f s = atomically $
  readTVar s >>= writeTVar s . (M.adjust f k)

writeNewSession :: SessionData -> TVar Sessions -> IO String
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

--
-- Config
--

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
    setup    <- lookup' "setup"
    library  <- lookup' "library"
    ignored  <- lookup' "ignored"
    ffmpeg   <- lookup' "ffmpeg"
    exts     <- lookup' "exts"
    bitrate  <- lookup' "bitrate"
    password <- lookup' "password"
    return $ Config setup library ignored ffmpeg exts bitrate password
    where
      assocs = fromJSObject obj
      lookup' k = maybe (Error "malformed config")
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

configFile :: IO FilePath
configFile = getHomeDirectory >>= return . (</> ".solanin/config")

emptyConfig :: Config
emptyConfig = Config
  { configSetup    = False
  , configLibrary  = ""
  , configIgnored  = []
  , configFFmpeg   = Nothing
  , configExts     = []
  , configBitrate  = 128
  , configPassword = ""
  }

newConfig :: IO (TVar Config)
newConfig = atomically $ newTVar emptyConfig

readConfig :: TVar Config -> IO Config
readConfig = atomically . readTVar

writeConfig :: Config -> TVar Config -> IO ()
writeConfig c x = atomically $ writeTVar x c

adjustConfig :: (Config -> Config) -> TVar Config -> IO ()
adjustConfig f x = atomically $ readTVar x >>= (writeTVar x) . f

loadConfig :: IO (TVar Config)
loadConfig = do
  fp     <- configFile
  result <- liftM decode (readFile fp)
  case result of
    Ok c    -> atomically $ newTVar c
    Error _ -> fail "malformed config"

saveConfig :: TVar Config -> IO ()
saveConfig c = do
  fp <- configFile
  createDirectoryIfMissing True (takeDirectory fp)
  readConfig c >>= writeFile fp . encode

sha1Crypt :: String -> String -> String
sha1Crypt salt plain = salt' ++ (show $ (sha1 . L.pack) (salt' ++ plain))
  where
    salt' = takeWhile (/= '$') salt ++ "$"

mkSalt :: Int -> IO String
mkSalt n = replicateM n (randomIdx >>= return . (hexL !!))
  where
    hexL = ['0'..'9'] ++ ['a'..'f']
    randomIdx = getStdRandom (randomR (0, length hexL - 1))

--
-- Index
--

-- Internal data structure used to calculate deltas between libraries to
-- rebuild the index.
data DirSnapshot = DirSnapshot ClockTime (M.Map FilePath ClockTime)
  deriving (Show, Eq)

instance B.Binary ClockTime where
  put (TOD s1 s2) = B.put s1 >> B.put s2
  get = liftM2 TOD B.get B.get

instance B.Binary DirSnapshot where
  put (DirSnapshot mtime fs) = B.put mtime >> B.put fs
  get = liftM2 DirSnapshot B.get B.get

instance Monoid DirSnapshot where
  mempty = DirSnapshot (TOD 0 0) M.empty
  mappend (DirSnapshot mtime fs) (DirSnapshot mtime' fs') =
    DirSnapshot (max mtime mtime') (M.unionWith max fs fs')

-- Internal data structure to rebuild the index.
data IndexChange = AddSong         Song
                 | UnionIndex      Index
                 | RemoveSong      ByteString
                 | RemoveDirectory ByteString
  deriving (Show, Eq)

-- Internal data structure used to do queries.
data QueryClause = QueryTitle  String
                 | QueryArtist String
                 | QueryAlbum  String
  deriving (Show, Eq)

data Index = Index
  { indexLibrary  :: FilePath
  , indexSnapshot :: T.Trie DirSnapshot
  , indexSongs    :: T.Trie Song
  , indexTitles   :: T.Trie (S.Set ByteString)
  , indexArtists  :: T.Trie (S.Set ByteString)
  , indexAlbums   :: T.Trie (S.Set ByteString)
  } deriving (Show, Eq)

instance B.Binary Index where
  put (Index lib snapshot songs titles artists albums) = do
    B.put indexMagic
    B.put lib
    B.put snapshot
    B.put songs
    B.put titles
    B.put artists
    B.put albums
  get = do
    magic <- B.get
    if magic == indexMagic then do
      lib      <- B.get
      snapshot <- B.get
      songs    <- B.get
      titles   <- B.get
      artists  <- B.get
      albums   <- B.get
      return $ Index lib snapshot songs titles artists albums
      else throw (userError "wrong index magic; please re-build the index")

indexMagic = "solanin0"

indexFile :: IO FilePath
indexFile = getHomeDirectory >>= return . (</> ".solanin/index.gz")

emptyIndex :: Index
emptyIndex = Index "" T.empty T.empty T.empty T.empty T.empty

unionIndex :: Index -> Index -> Index
unionIndex i i' = Index
  { indexLibrary  = if indexLibrary i == indexLibrary i' then
                      indexLibrary i
                      else throw (userError msg)
  , indexSnapshot = u' (indexSnapshot i) (indexSnapshot i')
  , indexSongs    = T.unionR (indexSongs i) (indexSongs i')

  , indexTitles   = u (indexTitles  i) (indexTitles  i')
  , indexArtists  = u (indexArtists i) (indexArtists i')
  , indexAlbums   = u (indexAlbums  i) (indexAlbums  i')
  }
  where
    u   = T.unionWith S.union
    u'  = T.unionWith mappend
    msg = "can't union indexes of different libraries"

scanDirectory :: [String] -> FilePath -> IO [FilePath]
scanDirectory igs d =
  liftM (\x -> map (d </>) (x \\ (igs ++ [".", ".."]))) $
  getDirectoryContents d

prefixes :: String -> [String]
prefixes [] = []
prefixes s  = s' : prefixes s''
  where
    s'  = dropWhile isSpace s
    s'' = snd (break isSpace s')

songsToIndex :: [Song] -> FilePath -> T.Trie DirSnapshot -> Index
songsToIndex ss lib snapshot = Index lib snapshot songs titles artists albums
  where
    songs   = T.fromList $
             [ (U.fromString (songPath s), s) | s <- ss ]
    titles  = fromSongs (prefixes . songTitle)
    artists = fromSongs (prefixes . songArtist)
    albums  = fromSongs (prefixes . songAlbum)

    fromSongs kf = foldr (T.unionWith S.union) T.empty (map f ss)
      where
        f s = T.fromList [ (normalizeKey k, v) | k <- kf s ]
          where
            v = S.singleton $ U.fromString (songPath s)

rebuildIndex :: Config -> TVar Index -> IO ()
rebuildIndex config idx = do
  index <- atomically (readTVar idx)
  if configLibrary config /= indexLibrary index then
    buildIndex config idx
    else do
      let snapshot = indexSnapshot index
      mdirs  <- filterM isModified (T.toList snapshot)
      hPutStrLn stderr "rebuilding index"
      index' <- foldrM (rescanDirectory snapshot) index mdirs
      atomically $ writeTVar idx index'
  where
    isModified (fp, DirSnapshot mtime _) = do
      de <- doesDirectoryExist (U.toString fp)
      if de then do
        mtime' <- getModificationTime (U.toString fp)
        return $ mtime' > mtime
        else return True

    changeNew fs snapshot (fp, mtime') =
      case M.lookup fp fs of
        Just mtime -> if mtime' > mtime then do
          s <- open exts fp
          let rm = RemoveSong (U.fromString fp)
          return $ maybe [rm] (\s' -> [rm, AddSong s']) s
          else return []
        Nothing    ->
          case T.lookup (U.fromString fp) snapshot of
            -- If the directory is already in the snapshot, it'll be taken
            -- care of later.
            Just _  -> return []
            Nothing -> do
              isd <- doesDirectoryExist fp
              if isd then do
                idx' <- buildIndex' config fp
                return [UnionIndex idx']
                else do
                  s <- open exts fp
                  return $ maybe [] (\s' -> [AddSong s']) s

    changeRemove fs' songs fp =
      case M.lookup fp fs' of
        Just _  -> []
        Nothing -> case T.keys (T.submap (U.fromString fp) songs) of
          [_] -> [RemoveSong (U.fromString fp)]
          _   -> throw (userError ("internal index error: " ++ fp))

    rescanDirectory snapshot (fp, DirSnapshot _ fs) index = do
      de <- doesDirectoryExist (U.toString fp)
      hPutStrLn stderr ((if de then "  [~] " else "  [-] ") ++
                        U.toString fp ++ "...")
      if de then do
        fs'   <- scanDirectory igs (U.toString fp) >>= mapM snapshotPair
        news  <- mapM (changeNew fs snapshot) fs'
        mtime <- getModificationTime (U.toString fp)
        let rms  = map (changeRemove (M.fromList fs') (indexSongs index))
                       (M.keys fs)
            chgs = concat news ++ concat rms
        ds <- mapM snapshotPairSong [ s | AddSong s <- chgs ]
        let snapshot' = T.insert fp (DirSnapshot mtime (M.fromList ds)) T.empty
            index'    = songsToIndex [ s | AddSong s <- chgs ] lib snapshot'
            index''   = foldr unionIndex index' [ i | UnionIndex i <- chgs ]
        return $ unionIndex (pruneIndex index chgs) index''
        else return $ pruneIndex index [RemoveDirectory fp]

    pruneIndex index [] = index
    pruneIndex index (c:cs) = case c of
      RemoveSong fp -> let
        snapshot' = rmFromSnapshot fp
        songs'    = rmFromSongs    fp
        titles'   = rmFromConcord  fp (prefixes . songTitle)  titles
        artists'  = rmFromConcord  fp (prefixes . songArtist) artists
        albums'   = rmFromConcord  fp (prefixes . songAlbum)  albums
        index'    = Index ilib snapshot' songs' titles' artists' albums'
        in pruneIndex index' cs
      RemoveDirectory fp -> let
        ks        = T.keys (T.submap fp songs)
        snapshot' = T.delete fp snapshot
        index'    = Index ilib snapshot' songs titles artists albums
        in pruneIndex index' ([ RemoveSong k | k <- ks ] ++ cs)
      _ -> pruneIndex index cs
      where
        rmFromSnapshot fp = T.adjust f k snapshot
          where
            fp' = U.toString fp
            k = U.fromString (takeDirectory fp')
            f (DirSnapshot mt fs) = DirSnapshot mt (M.delete fp' fs)
        rmFromSongs fp = T.delete fp songs
        rmFromConcord fp kf conc =
          foldr (T.update f) conc [ normalizeKey k | k <- kf s ]
          where
          Just s = T.lookup fp songs
          f ss = if S.null ss' then Nothing else Just ss'
            where ss' = S.delete fp ss

        ilib     = indexLibrary  index
        snapshot = indexSnapshot index
        songs    = indexSongs    index
        titles   = indexTitles   index
        artists  = indexArtists  index
        albums   = indexAlbums   index

    snapshotPairSong s = do
      mtime <- getModificationTime (songPath s)
      return (songPath s, mtime)

    snapshotPair fp = do
      mtime <- getModificationTime fp
      return (fp, mtime)

    lib  = configLibrary config
    exts = configExts config
    igs  = configIgnored config

buildIndex' :: Config -> FilePath -> IO Index
buildIndex' config r = loop r
  where
    loop d = do
      hPutStrLn stderr ("  [+] " ++ d ++ "...")
      fs    <- scanDirectory igs d
      mtime <- getModificationTime d
      dirs  <- filterM doesDirectoryExist fs
      ss    <- liftM catMaybes (mapM (open exts) fs)
      ds    <- mapM snapshotPair ss

      is <- mapM loop dirs
      let snapshot = T.insert (U.fromString d)
                              (DirSnapshot mtime (M.fromList ds)) T.empty
      return $ foldr unionIndex (songsToIndex ss lib snapshot) is

    snapshotPair s = do
      mtime <- getModificationTime (songPath s)
      return (songPath s, mtime)

    lib  = configLibrary config
    exts = configExts    config
    igs  = configIgnored config

buildIndex :: Config -> TVar Index -> IO ()
buildIndex config idx = do
  hPutStrLn stderr "building index"
  buildIndex' config lib >>= atomically . (writeTVar idx)
  where
    lib = configLibrary config

normalizeKey :: String -> ByteString
normalizeKey = U.fromString . normalizeString

newIndex :: IO (TVar Index)
newIndex = atomically $ newTVar emptyIndex

readIndex :: TVar Index -> IO Index
readIndex = atomically . readTVar

queryIndex :: String -> TVar Index -> IO (S.Set Song)
queryIndex q i = (atomically . readTVar) i >>=
  -- Special case for querying the whole library.
  if null q then return . S.fromList . T.elems . indexSongs
    else case parse clauses "" q of
      -- If no explicit clauses are used, assume all three.
      Left _   -> go [QueryArtist q, QueryTitle q, QueryAlbum q]
      Right cs -> go cs
  where
    clauses = sepEndBy1 clause spaces
    clause  =  p "artist:" QueryArtist
           <|> p "title:"  QueryTitle
           <|> p "album:"  QueryAlbum
      where
        p s con = string s >> spaces >> many1 c >>= return . con
        c = satisfy (not . isSpace)

    go cs idx = return $
                S.map (\fp -> fromJust (T.lookup fp (indexSongs idx))) $
                S.unions (map (query idx) cs)

    query idx (QueryArtist s) = lookup' s (indexArtists idx)
    query idx (QueryTitle  s) = lookup' s (indexTitles  idx)
    query idx (QueryAlbum  s) = lookup' s (indexAlbums  idx)

    lookup' k c = S.unions (T.elems c')
      where
        c' = T.submap (normalizeKey k) c

saveIndex :: TVar Index -> IO ()
saveIndex x = do
  fp <- indexFile
  createDirectoryIfMissing True (takeDirectory fp)
  (atomically . readTVar) x >>= L.writeFile fp . compress . B.encode

loadIndex :: IO (TVar Index)
loadIndex = do
  fp <- indexFile
  bs <- L.readFile fp
  -- Force entire stream to be consumed.
  evaluate $ L.length bs
  atomically $ newTVar (B.decode (decompress bs))
