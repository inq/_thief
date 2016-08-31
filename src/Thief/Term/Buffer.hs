module Thief.Term.Buffer
  ( Buffer(..)
  , blankBuffer
  , borderedBuffer
  ) where

import Prelude hiding (lines)
import Thief.Term.Brush (Brush(..))
import Thief.Term.Classes (Printable(..))
import Thief.Term.Line (Line(..), blankLine, borderedLine)

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

borderedBuffer :: Brush -> Brush -> Int -> Int -> Buffer
borderedBuffer ebr ibr w h
  | h >= 2 = MkBuffer $ [edge] ++ replicate (h - 2) inside ++ [edge]
  | h == 1 = MkBuffer [edge]
  | otherwise = MkBuffer []
  where
    edge = blankLine ebr w
    inside = borderedLine ebr ibr w
