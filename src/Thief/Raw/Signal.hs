module Thief.Raw.Signal where

import qualified Control.Concurrent.Chan as C
import qualified System.Posix.Signals as S
import qualified Thief.Raw.FFI    as FFI
import qualified Thief.Raw.Result as Res
import System.Posix.Signals.Exts (sigWINCH)

resizeScreen :: C.Chan Res.Result -> IO ()
resizeScreen c = do
    termSize <- FFI.getTermSize
    C.writeChan c $ Res.Action $ Res.ResizeScreen termSize

installHandlers :: C.Chan Res.Result -> IO ()
installHandlers c = do
    S.installHandler sigWINCH (S.Catch $ resizeScreen c) Nothing
    resizeScreen c
