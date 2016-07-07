module Thief.Status where

import Data.Char

data Status = Idle
            | Escape
            | Empty
            | Num Int
            | Pair Int Int
            | Result ResData
            deriving Show
data ResData = RPair Int Int
             deriving Show

char :: Status -> Char -> Maybe Status
char Idle '\ESC' = Just Escape
char Idle _ = Nothing
char Escape '[' = Just Empty
char Empty c
  | isDigit c = Just $ Num $ digitToInt c
  | otherwise = Nothing
char (Num n) c
  | isDigit c = Just $ Num (n * 10 + digitToInt c)
  | c == ';'  = Just $ Pair n 0
  | otherwise = Nothing
char (Pair a b) c
  | isDigit c = Just $ Pair a (b * 10 + digitToInt c)
  | c == 'R'  = Just $ Result $ RPair a b
  | otherwise = Nothing
char _ _ = Nothing
