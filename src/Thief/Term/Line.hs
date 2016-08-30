module Thief.Term.Line
  ( Line(..)
  ) where

import Prelude hiding (lines)
import Thief.Term.TChar (TChar)
import Thief.Color (Color(..))
import Thief.Term.Printable (Printable(..))

data Line = MkLine
  { chars :: [TChar]
  }

instance Printable Line where
  width = sum . map width . chars
  height _ = 1
