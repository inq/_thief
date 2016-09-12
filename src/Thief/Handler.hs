module Thief.Handler
  ( initLoop
  ) where

import Control.Monad (when)
import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Monad.State (StateT, runStateT, get, put, modify)
import Misc (def)
import Thief.Handler.Status (Status(..))
import Thief.UI.Screen (Screen, initScreen, drawScreen, diffScreen)
import Thief.UI.Common
  ( Drawable(..)
  , Size(..)
  , Responsable(event)
  , Editable(..)
  , Coord(..)
  , Result(Refresh)
  )
import Thief.Raw (Event(..))
import Thief.Term.Ansi (smcup, restore, queryCursorPos)

-- * Type Alises

type Handler = StateT Status (Writer String) ()

exit :: Handler
-- ^ Exit the handler
exit = put Terminated

throwError :: String -> Handler
-- ^ Exit with error
throwError msg = tell msg >> put Terminated

eventLoop :: Chan Event -> Int -> Int -> Screen -> IO ()
-- ^ The Main event loop
eventLoop c x y = loop
  where
    loop scr = do
      e <- readChan c
      case e of
        Char 'q' -> finalize
        _ -> do
          let (scr', act) = event scr e
          case act of
              [Refresh] -> putStr $ drawScreen scr'
              as -> putStr $ concatMap show as
          loop scr'
    finalize = putStr $ restore x y

initLoop :: Chan Event -> IO ()
-- ^ Initialization
initLoop c = receiveSize
  where
    receiveSize = do
      e <- readChan c
      case e of
        Resize w h -> do
          putStr queryCursorPos
          receiveCursorPos w h
        _ -> putStrLn "COULD NOT RECEIVE SIZE"
    receiveCursorPos w h = do
      e <- readChan c
      case e of
        Pair y x -> do
          putStr smcup
          let (scr, act) = event (initScreen def) $ Resize w h
          putStr $ drawScreen scr
          eventLoop c x y scr
        _ -> putStrLn "COULD NOT RECEIVE CURSOR"
