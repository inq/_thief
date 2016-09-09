module Thief.UI.Theme
  ( Theme(..)
  ) where

import Misc (Default(..), Color(..))
import Thief.Term.Brush (Brush(..))

-- * Data Constructors

data Theme = MkTheme
  { editor :: Brush
  }

instance Default Theme where
  def = MkTheme
    { editor = MkBrush (RGB 220 220 220) (RGB 50 0 0)
    }
