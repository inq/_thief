module Thief.UI.Common
  ( Size(..)
  , Coord(..)
  , Resizable(..)
  , Drawable(..)
  , Focusable(..)
  , Responsable(..)
  , Editable(..)
  , Result(..)
  ) where

import Thief.UI.Event (Event)
import Misc (Default(def))
import Thief.Term.Buffer (Buffer(..))

-- * Data Constructors

data Direction = DLeft | DRight | DUp | DDown

data Size = MkSize { getWidth :: Int, getHeight :: Int }

data Coord = MkCoord { getX :: Int, getY :: Int }

data Result
  = RMove Int Int
  | RChar Char
  | Refresh

instance Show Result where
  show (RMove i j) = horz i ++ vert j
    where
      horz x
        | x < 0 = "\ESC[" ++ show (-x) ++ "D"
        | x > 0 = "\ESC[" ++ show x ++ "C"
        | otherwise = ""
      vert y
        | y < 0 = "\ESC[" ++ show (-y) ++ "A"
        | y > 0 = "\ESC[" ++ show y ++ "B"
        | otherwise = ""
  show (RChar c) = [c]
  show Refresh = []

instance Default Coord where
  def = MkCoord 0 0

class Responsable a where
  event :: a -> Event -> (a, [Result])

class Editable a where
  findCursor :: a -> Coord

class Resizable a where
  resize :: a -> Size -> a

class Drawable a where
  draw :: a -> Buffer

class Focusable a where
  setFocus :: a -> a
  releaseFocus :: a -> a
