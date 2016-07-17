module Thief.Raw
  ( initialize
  , runLoop
  , Res.Result(..)
  , Res.Arrow(..)
  , Res.Action(..)
  ) where

import qualified Control.Concurrent.Chan as C
import qualified Thief.Raw.Result as Res
import qualified Thief.Raw.Input  as Ipt
import qualified Thief.Raw.FFI    as FFI
import qualified Thief.Status    as Stat
import qualified Thief.Color     as Color

initialize :: IO ()
initialize = do
    Ipt.initialize

runLoop :: C.Chan Res.Result -> IO ()
runLoop c = do
    termSize <- FFI.getTermSize
    C.writeChan c $ Res.Action $ Res.ResizeScreen termSize
    Ipt.inputLoop c Stat.defaultStatus
