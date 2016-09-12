module Thief.UI.Theme
  ( Theme(..)
  ) where

import Misc (Default(..), Color(..))
import Thief.Term.Brush (Brush(..))

-- * Data Constructors

data Theme = MkTheme
  { editor :: Brush
  , windowFocused :: Brush
  , windowUnFocused :: Brush
  }

instance Default Theme where
  def = MkTheme
    { editor = MkBrush (RGB 220 220 220) (RGB 50 0 0)
    , windowFocused = MkBrush (RGB 0 0 0) (RGB 220 100 100)
    , windowUnFocused = MkBrush (RGB 0 0 0) (RGB 100 100 220)
    }
