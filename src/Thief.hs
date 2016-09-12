module Thief
  ( mainLoop
  ) where

import Control.Concurrent.Chan (Chan, newChan)
import Control.Concurrent (forkIO)
import Thief.Raw (initialize, runLoop)
import Thief.Handler (initLoop)

mainLoop :: IO ()
-- ^ The main procedure
mainLoop = do
    initialize
    chan <- newChan
    _ <- forkIO $ runLoop chan
    initLoop chan
