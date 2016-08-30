module Thief.Term.TChar
  ( TChar(..)
  , width
  ) where

import Thief.Color (Brush(..))
import Thief.Term.FFI (wcWidth)
import Thief.Term.Printable (Printable(..))

data TChar = MkChar
  { ch :: Char
  , brush :: Brush
  }

instance Printable TChar where
  width MkChar { ch = ch } = wcWidth ch
  height MkChar {} = 1
