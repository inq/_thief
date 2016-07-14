module Thief.Ansi where

import qualified Thief.Color as Color

queryCursorPos :: String
queryCursorPos = "\ESC[6n"

clearScreen :: String
clearScreen = "\ESC[2J"

setBackground :: Color.Color -> String
setBackground c = "\ESC[48;2;" ++ show c ++ "m"

fillScreen :: Int -> Int -> Color.Color -> String
fillScreen w h c = clearScreen ++ setBackground c ++ take (w * h) (repeat ' ')
