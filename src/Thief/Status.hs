module Thief.Status where

import qualified Thief.Raw.Result as Res
import Data.Char

data Status = Idle
  | Escape
  | Empty
  | Num Int
  | Chr Char
  | Pair Int Int
  | Trio Int Int Int
  deriving Show

defaultStatus :: Status
defaultStatus = Idle

success :: Res.Result -> (Status, Res.Result)
success = (,) Idle

proceed :: Status -> (Status, Res.Result)
proceed s = (,) s Res.None

char :: Status -> Char -> (Status, Res.Result)
char Idle '\ESC' = proceed Escape
char Idle c
  | isLetter c = success $ Res.Char c
  | otherwise  = proceed $ Chr c
char Escape '[' = proceed Empty
char Empty c
  | isDigit c = proceed $ Num $ digitToInt c
  | c == 'A'  = success $ Res.Arrow Res.AUp
  | c == 'B'  = success $ Res.Arrow Res.ADown
  | c == 'C'  = success $ Res.Arrow Res.ARight
  | c == 'D'  = success $ Res.Arrow Res.ALeft
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
