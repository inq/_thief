module Thief.Term.Classes
  ( Printable (..)
  ) where

import Thief.Term.Brush (Brush)

class Printable p where
  width :: p -> Int
  height :: p -> Int
  toAnsi :: Brush -> p -> (Brush, String)
