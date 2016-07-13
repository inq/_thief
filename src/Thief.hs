module Thief where

import qualified Thief.Input   as Ipt
import qualified Thief.Output  as Opt
import qualified Thief.Status  as Stat

mainLoop :: IO ()
mainLoop = do
    Ipt.initialize
    Opt.initialize
    Ipt.inputLoop Stat.Idle
    Opt.finalize
