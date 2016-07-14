module Thief.Color where

data Color = RGB
  { red   :: Int
  , green :: Int
  , blue  :: Int
  }

darkBlood :: Color
darkBlood = RGB 80 0 0

instance Show Color where
    show (RGB r g b) = show r ++ ";" ++ show g ++ ";" ++ show b
