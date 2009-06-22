module Main where

import System.IO.Error (isUserError)
import Solanin.Server
import Solanin.State

main = do
  config   <- catch loadConfig (const newConfig)
  sessions <- newSessions
  serve solanin (config, sessions)
