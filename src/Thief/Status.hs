module Thief.Status where

import qualified Thief.IO.Result as Res
import Data.Char

data Status = Status
  { input :: InputStatus
  , cursorPos :: CursorPos
  }

data CursorPos = CursorPos
  { x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  }
  deriving Show

data InputStatus = Idle
  | Escape
  | Empty
  | Num Int
  | Chr Char
  | Pair Int Int
  | Trio Int Int Int
  deriving Show

moveUp :: CursorPos -> CursorPos
moveUp (CursorPos x' y' w h) = CursorPos x' (y' - 1) w h

moveDown :: CursorPos -> CursorPos
moveDown (CursorPos x' y' w h) = CursorPos x' (y' + 1) w h

moveLeft :: CursorPos -> CursorPos
moveLeft (CursorPos x' y' w h) = CursorPos (x' - 1) y' w h

moveRight :: CursorPos -> CursorPos
moveRight (CursorPos x' y' w h) = CursorPos (x' + 1) y' w h

defaultStatus :: Int -> Int -> Status
defaultStatus w h = Status Idle $ CursorPos 0 0 w h

success :: Res.Result -> (InputStatus, Res.Result)
success = (,) Idle

proceed :: InputStatus -> (InputStatus, Res.Result)
proceed s = (,) s Res.None

char :: InputStatus -> Char -> (InputStatus, Res.Result)
char Idle '\ESC' = proceed Escape
char Idle c
  | isLetter c = success $ Res.Char c
  | otherwise  = proceed $ Chr c
char Escape '[' = proceed Empty
char Empty c
  | isDigit c = proceed $ Num $ digitToInt c
  | c == 'A'  = success $ Res.Arrow Res.Up
  | c == 'B'  = success $ Res.Arrow Res.Down
  | c == 'C'  = success $ Res.Arrow Res.Right
  | c == 'D'  = success $ Res.Arrow Res.Left
char (Num n) c
  | isDigit c = proceed $ Num (n * 10 + digitToInt c)
  | c == ';'  = proceed $ Pair n 0
char (Pair a b) c
  | isDigit c = proceed $ Pair a (b * 10 + digitToInt c)
  | c == ';'  = proceed $ Trio a b 0
  | c == 'R'  = success $ Res.Pair a b
char (Trio a b c) d
  | isDigit d = proceed $ Trio a b (c * 10 + digitToInt d)
  | d == 't'  = success $ Res.Trio a b c
char _ _ = (Idle, Res.None)

toStr :: Res.Result -> String
toStr (Res.Char c)     = [c]
toStr (Res.Pair a b)   = "(" ++ show a ++ "," ++ show b ++ ")"
toStr (Res.Trio a b c) = "(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")"
toStr (Res.Arrow _)    = "*"
toStr Res.None         = "None"
