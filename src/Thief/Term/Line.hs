module Thief.Term.Line
  ( Line(..)
  , fromString
  , leftAligned
  ) where

import Prelude hiding (lines)
import Thief.Term.TChar (TChar(MkChar), space)
import Thief.Color (Brush(..))
import Thief.Term.Printable (Printable(..))

data Line = MkLine
  { chars :: [TChar]
  }

instance Printable Line where
  width = sum . map width . chars
  height _ = 1


fromString :: Brush -> String -> Line
fromString br str = MkLine $ map (MkChar br) str

leftAligned :: Brush -> Int -> String -> Line
leftAligned br w str = MkLine (leftAligned' br w str)
  where
    leftAligned' br w (h:t)
      | w > cw = nc : leftAligned' br (w - cw) t
      | otherwise = space br : leftAligned' br (w - 1) []
      where
        nc = MkChar br h
        cw = width nc
    leftAligned' br w [] = []
