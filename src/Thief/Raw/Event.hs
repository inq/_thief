module Thief.Raw.Event
  ( Event(..)
  ) where

-- * Data Constructors

data Event
  = KeyUp | KeyDown | KeyLeft | KeyRight
  | Pair Int Int
  | Trio Int Int Int
  | Char Char
  | Resize Int Int
  | Exit
  deriving (Eq, Show)
