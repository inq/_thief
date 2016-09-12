module Thief.Term.Ansi
  ( smcup
  , rmcup
  , civis
  , cvvis
  , queryCursorPos
  , clearScreen
  , moveCur
  , movexy
  , changeBrush
  , restore
  ) where

import Thief.Term.Brush (Brush(..))
import Thief.Term.Cursor (Cursor(..))
import Misc (Color)

restore :: Int -> Int -> String
-- ^ Restore the original screen & recover the cursor
restore x y = rmcup ++ movexy x y

smcup :: String
-- ^ Clear the screen
smcup = "\ESC[?47h"

rmcup :: String
-- ^ Restore the original screen
rmcup = "\ESC[?47l"

civis :: String
-- ^ Make cursor invisible
civis = "\ESC[?25l"

cvvis :: String
-- ^ Make cursor visible
cvvis = "\ESC[?25h"

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
