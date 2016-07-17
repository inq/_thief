module Thief where

import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent      as CC
import qualified Thief.Raw               as Raw
import qualified Thief.Handler           as Hdr

mainLoop :: IO ()
mainLoop = do
    chan <- C.newChan
    Raw.initialize
    tid <- CC.forkIO $ Raw.runLoop chan
    Hdr.handlerLoop chan
