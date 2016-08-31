module Thief.Term.Ansi
  ( smcup
  , rmcup
  , queryCursorPos
  , clearScreen
  , setBackground
  , fillScreen
  , moveCursor
  , move
  , spaces
  , changeBrush
  ) where

import Thief.Term.Brush (Brush(..))
import Thief.Term.Cursor (Cursor(..))
import Misc.Color (Color)

smcup :: String
smcup = "\ESC[?47h"

rmcup :: String
rmcup = "\ESC[?47l"

queryCursorPos :: String
queryCursorPos = "\ESC[6n"

clearScreen :: String
clearScreen = "\ESC[2J"

setBackground :: Color -> String
setBackground c = "\ESC[48;2;" ++ show c ++ "m"

fillScreen :: Int -> Int -> Color -> String
fillScreen w h c = clearScreen ++ setBackground c ++ replicate (w * h) ' '

moveCursor :: Cursor -> String
moveCursor MkCursor { getX = x, getY = y } = move x y

move :: Int -> Int -> String
move x y = "\ESC[" ++ show y ++ ";" ++ show x ++ "f"

spaces :: Color -> Int -> String
spaces c n = setBackground c ++ replicate n ' '

changeBrush :: Brush -> Brush -> String
changeBrush (MkBrush ef eb) (MkBrush nf nb) = chfg ++ chbg
  where
    chfg = if ef /= nf
      then "\ESC[38;2;" ++ show nf ++ "m"
      else ""
    chbg = if eb /= nb
      then "\ESC[48;2;" ++ show nb ++ "m"
      else ""
