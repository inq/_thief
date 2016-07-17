module Thief.IO.Result
  ( Result(..)
  , Arrow(..)
  ) where

data Result = Arrow Arrow
  | Pair Int Int
  | Trio Int Int Int
  | Char Char
  | None
  deriving (Eq, Show)

data Arrow = Up | Down | Left | Right
  deriving (Eq, Show)
