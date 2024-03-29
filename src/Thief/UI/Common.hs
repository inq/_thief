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

import Thief.Raw (Event)
import Misc (Default(def))
import Thief.Term.Buffer (Buffer(..))

-- * Data Constructors

data Direction = DLeft | DRight | DUp | DDown

data Size = MkSize { getWidth :: Int, getHeight :: Int }

data Coord = MkCoord { getX :: Int, getY :: Int }

data Result
  = RMoveUp Int
  | RMoveDown Int
  | RMoveLeft Int
  | RMoveRight Int
  | RChar Char
  | Refresh

instance Show Result where
  show (RMoveUp i) = "\ESC[" ++ show i ++ "A"
  show (RMoveDown i) = "\ESC[" ++ show i ++ "B"
  show (RMoveLeft i) = "\ESC[" ++ show i ++ "D"
  show (RMoveRight i) = "\ESC[" ++ show i ++ "C"
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
