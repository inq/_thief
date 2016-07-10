module Thief.Status where

import Data.Char

data Status = Idle
            | Escape
            | Empty
            | Num Int
            | Chr Char
            | Pair Int Int
            | Trio Int Int Int
            deriving Show
data ResData = RPair Int Int
             | RTrio Int Int Int
             | RChar Char
             | None
             deriving (Eq, Show)

success :: ResData -> (Status, ResData)
success = (,) Idle

proceed :: Status -> (Status, ResData)
proceed s = (,) s None

char :: Status -> Char -> (Status, ResData)
char Idle '\ESC' = proceed Escape
char Idle c
  | isLetter c = success $ RChar c
  | otherwise = proceed $ Chr c
char Escape '[' = proceed Empty
char Empty c
  | isDigit c = proceed $ Num $ digitToInt c
  | c == 'A'  = success $ RChar '↑'
  | c == 'B'  = success $ RChar '↓'
  | c == 'C'  = success $ RChar '→'
  | c == 'D'  = success $ RChar '←'
char (Num n) c
  | isDigit c = proceed $ Num (n * 10 + digitToInt c)
  | c == ';'  = proceed $ Pair n 0
char (Pair a b) c
  | isDigit c = proceed $ Pair a (b * 10 + digitToInt c)
  | c == ';'  = proceed $ Trio a b 0
  | c == 'R'  = success $ RPair a b
char (Trio a b c) d
  | isDigit d = proceed $ Trio a b (c * 10 + digitToInt d)
  | d == 't'  = success $ RTrio a b c
char _ _ = (Idle, None)

toStr :: ResData -> String
toStr (RChar c)     = [c]
toStr (RPair a b)   = "(" ++ (show a) ++ "," ++ (show b) ++ ")"
toStr (RTrio a b c) = "(" ++ (show a) ++ "," ++ (show b) ++ "," ++ (show c) ++ ")"
toStr None          = "None"
