module Thief.Term.Printable
  ( Printable (..)
  ) where

import Thief.Color (Brush)

class Printable p where
  width :: p -> Int
  height :: p -> Int
  toAnsi :: Brush -> p -> (Brush, String)
