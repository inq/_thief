module Thief.UI.Common
  ( Size(..)
  , Resizable(..)
  , Drawable(..)
  , Focusable(..)
  ) where

import Thief.Term.Buffer (Buffer(..))

-- * Data Constructors

data Size = MkSize { getWidth :: Int, getHeight :: Int }

class Resizable a where
  resize :: a -> Size -> a

class Drawable a where
  draw :: a -> Buffer

class Focusable a where
  setFocus :: a -> a
  releaseFocus :: a -> a
