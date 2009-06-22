{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}

module Solanin.Validate (Validation(Valid, Invalid),
                         Validator,

                         valids,
                         invalids,
                         validate,

                         vAny,
                         vEqual,
                         vNotBlank,
                         vNumber,
                         vLibrary,
                         vFFmpeg,
                         vPassword) where

import Data.Char (isNumber)
import Control.Monad
import Control.Monad.Trans (MonadIO, liftIO)
import System.Directory
import System.Process
import System.Exit
import Solanin.State

-- VacuouslyValid should never be used explicitly
data Validation a = Valid a | Invalid String | VacuouslyValid
  deriving (Show, Eq)

instance Monad Validation where
  return a        = Valid a
  fail x          = Invalid x
  Valid a >>= f   = f a
  Invalid x >>= _ = Invalid x

instance MonadPlus Validation where
  mzero = VacuouslyValid

  -- the sum of a list of Validations is:
  --  * if there are any Invalids, first Invalid
  --  * if everything is Valid, the last Valid
  --  * if empty, VacuouslyValid
  Invalid x `mplus` ys             = Invalid x
  Valid x   `mplus` VacuouslyValid = Valid x
  xs        `mplus` ys             = ys

type Validator a = (MonadIO m) => a -> m (Validation a)

fromValid :: Validation a -> a
fromValid (Valid a) = a
fromValid _         = error "Solanin.Validate.fromValid: not valid"

valids :: [(String, Validation a)] -> [(String, a)]
valids vs = [ (l, a) | (l, Valid a) <- vs ]

invalids :: [(String, Validation a)] -> [(String, String)]
invalids vs = [ (l, x) | (l, Invalid x) <- vs ]

validate :: (MonadIO m)
         => [(String, a)]
         -> [(String, [Maybe a -> m (Validation (Maybe a))])]
         -> m [(String, Validation (Maybe a))]
validate form validators = liftM (zip (map fst validators)) mvals
  where
    msumM = liftM msum
    f k v = v (lookup k form)
    mvals = sequence [ msumM (mapM (f k) vs) | (k, vs) <- validators ]

vAny :: Validator a
vAny a = return (Valid a)

vEqual :: (Eq a) => a -> Validator a
vEqual a b =
  if a == b then
    return (Valid a)
    else return (Invalid "doesn't match")

vNotBlank :: Validator (Maybe String)
vNotBlank a@(Just s) =
  if null s then
    return (Invalid "cannot be blank")
    else return (Valid a)
vNotBlank Nothing  = return (Invalid "cannot be blank")

vNumber :: Validator (Maybe String)
vNumber a@(Just n) =
  if and $ map isNumber n then
    return (Valid a)
    else return (Invalid "must be a number")
vNumber Nothing = return (Invalid "malformed input")

vLibrary :: Validator (Maybe String)
vLibrary a@(Just lib) = liftIO $ do
  de <- doesDirectoryExist lib
  if de then do
    perms <- getPermissions lib
    if readable perms then
      return (Valid a)
      else return (Invalid "must be readable")
    else return (Invalid "does not exist")
vLibrary Nothing = return (Invalid "malformed input")

vFFmpeg :: Validator (Maybe String)
vFFmpeg a@(Just bin) = liftIO $ do
  if (null bin) then return (Valid a)
    else do
      (ec, out, err) <- readProcessWithExitCode bin ["-version"] ""
      case ec of
        ExitSuccess -> do
          if isFFmpeg (lines out) then return (Valid a)
            else return (Invalid "binary not FFmpeg")
        ExitFailure _ -> return (Invalid "error executing binary")
  where
    isFFmpeg [] = False
    isFFmpeg (l:ls) = case words l of
      []   -> False
      w:ws -> w == "FFmpeg"
vFFmpeg Nothing = return (Valid Nothing)

vPassword :: String -> Validator (Maybe String)
vPassword crypt a@(Just plain) =
  if sha1Crypt crypt plain == crypt then
    return (Valid a)
    else return (Invalid "wrong password")
vPassword _ Nothing = return (Invalid "wrong password")
