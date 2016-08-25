module Thief.Term.TermChar
  ( TermChar(..)
  , width
  ) where

import Thief.Color (Color(..))
import Thief.Term.FFI (wcWidth)
import Thief.Term.Printable (Printable(..))

data TermChar = TC
  { ch :: Char
  , bg :: Color
  , fg :: Color
  }

instance Printable TermChar where
  width TC { ch = ch } = wcWidth ch
  height TC { } = 1
