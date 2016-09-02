module Thief.Term.Brush
  ( Brush (..)
  , invertBrush
  ) where

import Misc (Default(def), Color, lightGray, darkGray)

-- * Data Constructors

data Brush = MkBrush
  { fg :: Color
  , bg :: Color
  } deriving (Eq, Show)

instance Default Brush where
    def = MkBrush lightGray darkGray

-- * Brush

invertBrush :: Brush -> Brush
-- ^ Swap fg with bg
invertBrush (MkBrush f b) = MkBrush b f
