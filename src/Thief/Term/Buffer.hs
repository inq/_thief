module Thief.Term.Buffer
  ( Buffer(..)
  ) where

import Prelude hiding (lines)
import Thief.Color (Brush(..))
import Thief.Term.Printable (Printable(..))
import Thief.Term.Line (Line(..))

data Buffer = MkBuffer
  { lines :: [Line]
  }

instance Printable Buffer where
  width = maximum . map width . lines
  height = length . lines
