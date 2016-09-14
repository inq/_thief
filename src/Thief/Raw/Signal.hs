module Thief.Raw.Signal where

import Control.Concurrent.Chan (Chan, writeChan)
import System.Posix.Signals.Exts (sigWINCH)
import System.Posix.Signals (Handler(Catch), installHandler, sigINT)
import Thief.Raw.FFI (getTermSize)
import Thief.Raw.Event (Event(Resize, Char))


resizeScreen :: Chan Event -> IO ()
-- ^ Send the resize signal to the handler
resizeScreen c = do
    (w, h) <- getTermSize
    writeChan c $ Resize w h

keyboardSignal :: Chan Event -> IO ()
-- ^ Send the resize signal to the handler
keyboardSignal c = do
    writeChan c $ Char 'A'

installHandlers :: Chan Event -> IO ()
-- ^ Install the signal handlers
installHandlers c = do
    installHandler sigWINCH (Catch $ resizeScreen c) Nothing
    installHandler sigINT (Catch $ keyboardSignal c) Nothing
    resizeScreen c
