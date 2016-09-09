module Thief.Raw.Signal where

import Control.Concurrent.Chan (Chan, writeChan)
import System.Posix.Signals.Exts (sigWINCH)
import System.Posix.Signals (Handler(Catch), installHandler)
import Thief.Raw.FFI (getTermSize)
import Thief.Raw.Event (Event(ResizeScreen))


resizeScreen :: Chan Event -> IO ()
-- ^ Send the resize signal to the handler
resizeScreen c = do
    termSize <- getTermSize
    writeChan c $ ResizeScreen termSize

installHandlers :: Chan Event -> IO ()
-- ^ Install the signal handlers
installHandlers c = do
    installHandler sigWINCH (Catch $ resizeScreen c) Nothing
    resizeScreen c
