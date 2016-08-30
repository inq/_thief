module Thief.Color
  ( Color (..)
  , Brush (..)
  , darkBlood
  , darkBlue
  , lightGray
  , darkGray
  ) where

import Misc.Default (Default(..))

data Color = RGB
  { red   :: Int
  , green :: Int
  , blue  :: Int
  }

data Brush = MkBrush { fg :: Color, bg :: Color }

instance Show Color where
    show (RGB r g b) = show r ++ ";" ++ show g ++ ";" ++ show b

instance Default Brush where
    def = MkBrush lightGray darkGray

darkBlood :: Color
darkBlood = RGB 80 0 0

darkBlue :: Color
darkBlue = RGB 0 0 80

lightGray :: Color
lightGray = RGB 200 200 200

darkGray :: Color
darkGray = RGB 50 50 50
