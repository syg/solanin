module Main where

import Solanin.Server
import Solanin.State

main = do
  config   <- catch loadConfig (const newConfig)
  sessions <- newSessions
  index    <- catch loadIndex (const newIndex)
  serve solanin $ State config sessions index
