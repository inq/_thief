module Misc.Color
  ( Color (..)
  , darkBlood
  , darkBlue
  , lightGray
  , darkGray
  ) where


data Color = RGB
  { red   :: Int
  , green :: Int
  , blue  :: Int
  } deriving (Eq)

instance Show Color where
    show (RGB r g b) = show r ++ ";" ++ show g ++ ";" ++ show b

darkBlood :: Color
darkBlood = RGB 80 0 0

darkBlue :: Color
darkBlue = RGB 0 0 80

lightGray :: Color
lightGray = RGB 200 200 200

darkGray :: Color
darkGray = RGB 50 50 50
