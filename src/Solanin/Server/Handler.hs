{-# LANGUAGE ImpredicativeTypes #-}

module Solanin.Server.Handler where

import qualified Data.ByteString.Char8 as C
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Concurrent.STM (TVar)
import Network.Wai
import Solanin.Server.Util
import Solanin.State

type HandlerT a = ReaderT (Environment, State) (MaybeT IO) a
type Handler    = HandlerT (Int, C.ByteString, Headers, Enumerator)

method :: Method -> Handler -> Handler
method m h = do
  env <- askEnvironment
  if (requestMethod env) == m then h else mzero

pathWith :: (C.ByteString -> C.ByteString -> Bool)
         -> String
         -> Handler
         -> Handler
pathWith c p h = do
  env <- askEnvironment
  if c p' (pathInfo env) then local f h else mzero
  where
    p' = C.pack p
    f (env, st) = (env { pathInfo = pathInfo' }, st)
      where
        pathInfo' = C.drop (C.length p') (pathInfo env)

prefix :: String -> Handler -> Handler
prefix = pathWith C.isPrefixOf

path :: String -> Handler -> Handler
path = pathWith (==)

setup :: Bool -> Handler -> Handler
setup b h = do
  config <- askConfig >>= liftIO . readConfig
  if (configSetup config) == b then h else mzero

authenticated' :: Bool -> Handler -> Handler -> Handler
authenticated' b h h' = do
  cookies <- liftM parseCookies' askEnvironment
  case lookup "sid" cookies of
    Just sid -> do
     sd <- askSessions >>= liftIO . (readSession sid)
     case sd of
       Just sd' -> if (sessionLoggedIn sd') == b then h else mzero
       Nothing  -> h'
    Nothing  -> if b then mzero else h

authenticated :: Bool -> Handler -> Handler
authenticated b h = authenticated' b h (if b then mzero else h)

currentSession :: HandlerT (Maybe SessionData)
currentSession = do
  cookies <- liftM parseCookies' askEnvironment
  case lookup "sid" cookies of
    Just sid -> askSessions >>= liftIO . (readSession sid)
    Nothing  -> return Nothing

askEnvironment :: HandlerT Environment
askEnvironment = asks fst

askConfig :: HandlerT (TVar Config)
askConfig = liftM stateConfig (asks snd)

askSessions :: HandlerT (TVar Sessions)
askSessions = liftM stateSessions (asks snd)

askIndex :: HandlerT (TVar Index)
askIndex = liftM stateIndex (asks snd)
