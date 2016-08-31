module Thief.Term.Buffer
  ( Buffer(..)
  , blankBuffer
  ) where

import Prelude hiding (lines)
import Thief.Term.Brush (Brush(..))
import Thief.Term.Printable (Printable(..))
import Thief.Term.Line (Line(..), blankLine)

data Buffer = MkBuffer
  { lines :: [Line]
  }

instance Printable Buffer where
  width = maximum . map width . lines
  height = length . lines
  toAnsi br (MkBuffer lines) = foldl convLine (br, "") lines
    where
      convLine (b, s) c = (nb, s ++ nc)
        where (nb, nc) = toAnsi b c

blankBuffer :: Brush -> Int -> Int -> Buffer
blankBuffer br w h = MkBuffer $ replicate h $ blankLine br w
