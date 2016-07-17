module Thief where

import qualified Control.Concurrent.Chan as C
import qualified Thief.IO           as IO

mainLoop :: IO ()
mainLoop = do
    chan <- C.newChan
    IO.initialize
    IO.runLoop chan
