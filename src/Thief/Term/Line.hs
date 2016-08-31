module Thief.Term.Line
  ( Line(..)
  , blankLine
  , borderedLine
  , fromString
  , leftAligned
  ) where

import Prelude hiding (lines)
import Thief.Term.Brush (Brush(..))
import Thief.Term.Classes (Printable(..))
import Thief.Term.TChar (TChar(MkChar), space)

data Line = MkLine
  { chars :: [TChar]
  }

instance Printable Line where
  width = sum . map width . chars
  height _ = 1
  toAnsi br (MkLine chars) = foldl convChar (br, "") chars
    where
      convChar (b, s) c = (nb, s ++ nc)
        where (nb, nc) = toAnsi b c

instance Monoid Line where
  mempty = MkLine []
  mappend (MkLine a) (MkLine b) = MkLine $ mappend a b
  mconcat ls = MkLine $ mconcat (chars <$> ls)


blankLine :: Brush -> Int -> Line
blankLine br n = MkLine $ replicate n $ MkChar br ' '

borderedLine :: Brush -> Brush -> Int -> Line
borderedLine ebr ibr w
  | w >= 2 = MkLine $ [edge] ++ replicate (w - 2) inside ++ [edge]
  | w == 1 = MkLine [edge]
  | otherwise = MkLine []
  where
    edge = MkChar ebr ' '
    inside = MkChar ibr ' '

fromString :: Brush -> String -> Line
fromString br str = MkLine (MkChar br <$> str)

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
