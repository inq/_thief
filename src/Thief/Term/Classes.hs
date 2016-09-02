module Thief.Term.Classes
  ( Printable (..)
  ) where

import Thief.Term.Brush (Brush)

-- * Class Definitions

class Printable p where
  width :: p -> Int
  height :: p -> Int
  toAnsi :: Brush -> p -> (Brush, String)
