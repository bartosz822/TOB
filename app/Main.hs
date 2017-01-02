module Main where

import           Control.Exception
import           Control.Monad.Reader
import           Bot
import           Network
import           System.IO


-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop       = runReaderT run


