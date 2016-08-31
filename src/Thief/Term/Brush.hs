module Thief.Term.Brush
  ( Brush (..)
  , invert
  ) where

import Misc.Color (Color(..), lightGray, darkGray)
import Misc.Default (Default(..))

data Brush = MkBrush
  { fg :: Color
  , bg :: Color
  } deriving (Eq, Show)

instance Default Brush where
    def = MkBrush lightGray darkGray

invert :: Brush -> Brush
invert (MkBrush f b) = MkBrush b f
