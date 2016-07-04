module Thief where

import qualified Thief.Input   as Ipt
import qualified Thief.Output  as Opt
import qualified Control.Monad as M

mainLoop :: IO ()
mainLoop = do
    Ipt.initialize
    Opt.initialize
    M.forever Ipt.inputLoop
