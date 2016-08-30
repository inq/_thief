module Thief.Color
  ( Color (..)
  , Brush (..)
  , darkBlood
  , darkBlue
  , lightGray
  ) where

data Color = RGB
  { red   :: Int
  , green :: Int
  , blue  :: Int
  }

data Brush = MkBrush { fg :: Color, bg :: Color }

darkBlood :: Color
darkBlood = RGB 80 0 0

darkBlue :: Color
darkBlue = RGB 0 0 80

lightGray :: Color
lightGray = RGB 200 200 200

instance Show Color where
    show (RGB r g b) = show r ++ ";" ++ show g ++ ";" ++ show b
