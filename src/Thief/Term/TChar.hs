module Thief.Term.TChar
  ( TChar(..)
  , space
  ) where

import Thief.Color (Brush(..))
import Thief.Term.FFI (wcWidth)
import Thief.Term.Printable (Printable(..))

data TChar = MkChar
  { brush :: Brush
  , ch :: Char
  }

instance Printable TChar where
  width MkChar { ch = ch } = wcWidth ch
  height MkChar {} = 1


space :: Brush -> TChar
space br = MkChar br ' '
