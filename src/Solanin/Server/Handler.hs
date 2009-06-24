{-# LANGUAGE ImpredicativeTypes #-}

module Solanin.Server.Handler where

import qualified Data.ByteString.Char8 as C
import Control.Monad.Maybe
import Control.Monad.Reader
import Network.Wai
import Solanin.Server.Util
import Solanin.State

type Handler = ReaderT (Environment, State)
                       (MaybeT IO)
                       (Int, C.ByteString, Headers, Enumerator)

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
     auth <- askSessions >>= liftIO . (readSession sid)
     case auth of
       Just auth' -> if auth' == b then h else mzero
       Nothing    -> h'
    Nothing  -> if b then mzero else h

authenticated :: Bool -> Handler -> Handler
authenticated b h = authenticated' b h (if b then mzero else h)

askEnvironment = asks fst
askConfig      = liftM fst (asks snd)
askSessions    = liftM snd (asks snd)
