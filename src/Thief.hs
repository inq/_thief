module Thief where

import qualified Thief.Input as Ipt
import qualified Control.Monad as M

mainLoop :: IO ()
mainLoop = do
    Ipt.initialize
    M.forever Ipt.inputLoop
