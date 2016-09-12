module Thief.UI.Common
  ( Size(..)
  , Coord(..)
  , Resizable(..)
  , Drawable(..)
  , Focusable(..)
  , Editable(..)
  ) where

import Misc (Default(def))
import Thief.Term.Buffer (Buffer(..))

-- * Data Constructors

data Direction = DLeft | DRight | DUp | DDown

data Size = MkSize { getWidth :: Int, getHeight :: Int }

data Coord = MkCoord { getX :: Int, getY :: Int }

instance Default Coord where
  def = MkCoord 0 0

class Editable a where
  findCursor :: a -> Coord

class Resizable a where
  resize :: a -> Size -> a

class Drawable a where
  draw :: a -> Buffer

class Focusable a where
  setFocus :: a -> a
  releaseFocus :: a -> a
