module Thief.Raw.Signal where

import Control.Concurrent.Chan (Chan, writeChan)
import System.Posix.Signals.Exts (sigWINCH)
import System.Posix.Signals (Handler(Catch), installHandler)
import Thief.Raw.FFI (getTermSize)
import Thief.Raw.Result (Result(Action), Action(ResizeScreen))


resizeScreen :: Chan Result -> IO ()
-- ^ Send the resize signal to the handler
resizeScreen c = do
    termSize <- getTermSize
    writeChan c $ Action $ ResizeScreen termSize

installHandlers :: Chan Result -> IO ()
-- ^ Install the signal handlers
installHandlers c = do
    installHandler sigWINCH (Catch $ resizeScreen c) Nothing
    resizeScreen c
