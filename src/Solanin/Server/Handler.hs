{-# LANGUAGE ImpredicativeTypes #-}

module Solanin.Server.Handler where

import Prelude hiding (drop, length)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, drop, length, isPrefixOf)
import Control.Concurrent.STM
import Control.Monad.Maybe
import Control.Monad.Reader
import Network.Wai
import Solanin.Server.Util
import Solanin.State

type Handler = ReaderT (Environment, State)
                       (MaybeT IO)
                       (Int, ByteString, Headers, Enumerator)

method :: Method -> Handler -> Handler
method m h = do
  env <- askEnvironment
  if (requestMethod env) == m then h else mzero

pathWith :: (ByteString -> ByteString -> Bool) -> String -> Handler -> Handler
pathWith pred p h = do
  env <- askEnvironment
  if pred p' (pathInfo env) then local f h else mzero
  where
    p' = pack p
    f (env, st) = (env { pathInfo = drop (length p') (pathInfo env) }, st)

prefix :: String -> Handler -> Handler
prefix = pathWith isPrefixOf

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
