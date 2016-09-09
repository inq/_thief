module Thief.Term.Cursor
  ( Cursor (..)
  , moveCursor
  ) where

import Misc (Default(def))
import Thief.Raw (Event(KeyUp, KeyDown, KeyLeft, KeyRight))
import Thief.Term.Brush (Brush)

-- * Data Constructors

data Cursor = MkCursor
  { theBrush :: Brush
  , theX :: Int
  , theY :: Int
  , theWidth :: Int
  , theHeight :: Int
  } deriving Show

instance Default Cursor where
  def = MkCursor def 0 0 0 0

-- * Cursor

moveCursor :: Cursor -> Event -> Cursor
-- ^ Modify the cursor by the action
moveCursor c@MkCursor { theX = x', theY = y' } = move'
  where
    move' KeyUp    = c { theY = y' - 1}
    move' KeyDown  = c { theY = y' + 1}
    move' KeyLeft  = c { theX = x' - 1}
    move' KeyRight = c { theX = x' + 1}
    move' _ = c
