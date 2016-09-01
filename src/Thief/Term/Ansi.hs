module Thief.Term.Ansi
  ( smcup
  , rmcup
  , queryCursorPos
  , clearScreen
  , moveCur
  , movexy
  , changeBrush
  ) where

import Thief.Term.Brush (Brush(..))
import Thief.Term.Cursor (Cursor(..))
import Misc.Color (Color)

smcup :: String
-- ^ Clear the screen
smcup = "\ESC[?47h"

rmcup :: String
-- ^ Restore the original screen
rmcup = "\ESC[?47l"

queryCursorPos :: String
-- ^ Query the cursor position to the client
queryCursorPos = "\ESC[6n"

clearScreen :: String
-- ^ Clean the screen
clearScreen = "\ESC[2J"

moveCur :: Cursor -> String
-- ^ Move the cursor
moveCur MkCursor { theX = x, theY = y } = movexy x y

movexy :: Int -> Int -> String
-- ^ Move the cursor (raw)
movexy x y = "\ESC[" ++ show y ++ ";" ++ show x ++ "f"

changeBrush :: Brush -> Brush -> String
-- ^ Change the brush
changeBrush (MkBrush ef eb) (MkBrush nf nb) = chfg ++ chbg
  where
    chfg = if ef /= nf
      then "\ESC[38;2;" ++ show nf ++ "m"
      else ""
    chbg = if eb /= nb
      then "\ESC[48;2;" ++ show nb ++ "m"
      else ""
