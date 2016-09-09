module Thief.Raw
  ( initialize, runLoop
  , Event(..)
  ) where

import Misc (def)
import Thief.Raw.Input (inputLoop, initialize)
import Control.Concurrent.Chan (Chan)
import Thief.Raw.Signal (installHandlers)
import Thief.Raw.Event (Event(..))

runLoop :: Chan Event -> IO ()
-- ^ Install the signal handlers and run the input loop
runLoop c = do
    installHandlers c
    inputLoop c
