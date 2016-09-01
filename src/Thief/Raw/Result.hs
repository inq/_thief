module Thief.Raw.Result
  ( Result(..)
  , Arrow(..)
  , Action(..)
  ) where

-- * Data Constructors

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

data Arrow = AUp | ADown | ALeft | ARight
  deriving (Eq, Show)
