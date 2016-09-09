module Thief.Term.Buffer
  ( Buffer(..)
  , blankBuffer
  , borderedBuffer
  , overlayBuffer
  ) where

import Thief.Term.Brush (Brush(..))
import Thief.Term.Classes (Printable(..))
import Thief.Term.Line (Line(..), blankLine, borderedLine, overlayLine)

-- * Data Constructors

data Buffer = MkBuffer
  { getLines :: [Line]
  }

instance Printable Buffer where
  width = maximum . map width . getLines
  height = length . getLines
  toAnsi br (MkBuffer getLines) = foldl convLine (br, "") getLines
    where
      convLine (b, s) c = (nb, s ++ nc)
        where (nb, nc) = toAnsi b c

-- * Buffer

blankBuffer :: Brush -> Int -> Int -> Buffer
-- ^ Make a rectangular buffer
blankBuffer br w h = MkBuffer $ replicate h $ blankLine br w

overlayBuffer :: Buffer -> Int -> Int -> Buffer -> Buffer
-- ^ Draw a buffer on a buffer
overlayBuffer (MkBuffer dst) x y (MkBuffer src) =
    MkBuffer $ overlay dst x y src
  where
    overlay (d:ds) x y (s:ss)
      | y > 0     = d : overlay ds x (y - 1) (s:ss)
      | otherwise = overlayLine d x s : overlay ds x 0 ss
    overlay d _ _ [] = d
    overlay [] _ _ _ = []


borderedBuffer :: Brush -> Brush -> Int -> Int -> Buffer
-- ^ Make a bordered rectangular buffer
borderedBuffer ebr ibr w h
  | h >= 2 = MkBuffer $ [edge] ++ replicate (h - 2) inside ++ [edge]
  | h == 1 = MkBuffer [edge]
  | otherwise = MkBuffer []
  where
    edge = blankLine ebr w
    inside = borderedLine ebr ibr w
