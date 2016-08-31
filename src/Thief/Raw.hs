module Thief.Raw
  ( initialize
  , runLoop
  , Result(..)
  , Arrow(..)
  , Action(..)
  ) where

import qualified Thief.Raw.Input as Ipt
import Control.Concurrent.Chan (Chan)
import Thief.Raw.Result (Result(..), Arrow(..), Action(..))
import Thief.Raw.Signal (installHandlers)
import Thief.Status (defaultStatus)


initialize :: IO ()
initialize = Ipt.initialize

runLoop :: Chan Result -> IO ()
runLoop c = do
    installHandlers c
    Ipt.inputLoop c defaultStatus
