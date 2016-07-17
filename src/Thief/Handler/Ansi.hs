module Thief.Handler.Ansi
  ( smcup
  , rmcup
  , queryCursorPos
  , clearScreen
  , setBackground
  , fillScreen
  , moveCursor
  , move
  , spaces
  ) where

import qualified Thief.Color          as Color
import qualified Thief.Handler.Cursor as Cur

smcup :: String
smcup = "\ESC[?47h"

rmcup :: String
rmcup = "\ESC[?47l"

queryCursorPos :: String
queryCursorPos = "\ESC[6n"

clearScreen :: String
clearScreen = "\ESC[2J"

setBackground :: Color.Color -> String
setBackground c = "\ESC[48;2;" ++ show c ++ "m"

fillScreen :: Int -> Int -> Color.Color -> String
fillScreen w h c = clearScreen ++ setBackground c ++ take (w * h) (repeat ' ')

moveCursor :: Cur.Cursor -> String
moveCursor (Cur.Cursor x y _ _) = move x y

move :: Int -> Int -> String
move x y = "\ESC[" ++ show y ++ ";" ++ show x ++ "f"

spaces :: Color.Color -> Int -> String
spaces c n = setBackground c ++ take n (repeat ' ')
