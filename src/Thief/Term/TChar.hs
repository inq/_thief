module Thief.Term.TChar
  ( TChar(..)
  , space
  ) where

import Thief.Term.Brush (Brush(..))
import Thief.Term.Classes (Printable(..))
import Thief.Term.FFI (wcWidth)
import Thief.Term.Ansi (changeBrush)

-- * Data Constructors

data TChar = MkChar
  { brush :: Brush
  , ch :: Char
  }

instance Printable TChar where
  width MkChar { ch = ch } = wcWidth ch
  height MkChar {} = 1
  toAnsi b' (MkChar b c) = (b, changeBrush b' b ++ [c])

-- * TChar

space :: Brush -> TChar
-- ^ Make a TChar for empty space
space br = MkChar br ' '
