module Thief.Raw.Result
  ( Result(..)
  , Arrow(..)
  , Action(..)
  ) where

data Result = Arrow Arrow
  | Pair Int Int
  | Trio Int Int Int
  | Char Char
  | Action Action
  | None
  deriving (Eq, Show)

data Action = Exit
  | ResizeScreen (Maybe (Int, Int))
  deriving (Eq, Show)

data Arrow = Up | Down | Left | Right
  deriving (Eq, Show)
