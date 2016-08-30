module Thief.Raw
  ( initialize
  , runLoop
  , Res.Result(..)
  , Res.Arrow(..)
  , Res.Action(..)
  ) where

import qualified Control.Concurrent.Chan as C
import qualified Thief.Raw.Signal as Sig
import qualified Thief.Raw.Result as Res
import qualified Thief.Raw.Input  as Ipt
import qualified Thief.Status    as Stat

initialize :: IO ()
initialize = Ipt.initialize

runLoop :: C.Chan Res.Result -> IO ()
runLoop c = do
    Sig.installHandlers c
    Ipt.inputLoop c Stat.defaultStatus
