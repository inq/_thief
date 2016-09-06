module Thief.Raw
  ( initialize, runLoop
  , Result(..)
  , Arrow(..)
  , Action(..)
  ) where

import Misc (def)
import Thief.Raw.Input (inputLoop, initialize)
import Control.Concurrent.Chan (Chan)
import Thief.Raw.Result (Result(..), Arrow(..), Action(..))
import Thief.Raw.Signal (installHandlers)


runLoop :: Chan Result -> IO ()
-- ^ Install the signal handlers and run the input loop
runLoop c = do
    installHandlers c
    inputLoop c
