module Thief where

import qualified Thief.Input   as Ipt
import qualified Thief.Output  as Opt
import qualified Thief.Status  as Stat

mainLoop :: IO ()
mainLoop = do
    Opt.initialize
    Ipt.initialize
    Ipt.inputLoop Stat.Idle
    Opt.finalize
