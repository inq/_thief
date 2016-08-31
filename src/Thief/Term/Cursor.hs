module Thief.Term.Cursor
  ( Cursor (..)
  , move
  ) where

import Misc.Default (Default(..))
import Thief.Raw (Arrow(..), Result(..))
import Thief.Term.Brush (Brush)

data Cursor = MkCursor
  { getBrush :: Brush
  , getX :: Int
  , getY :: Int
  , getWidth :: Int
  , getHeight :: Int
  } deriving Show

instance Default Cursor where
  def = MkCursor def 0 0 0 0

move :: Cursor -> Result -> Cursor
move c@MkCursor { getX = x', getY = y' } = move'
  where
    move' (Arrow AUp)    = c { getY = y' - 1}
    move' (Arrow ADown)  = c { getY = y' + 1}
    move' (Arrow ALeft)  = c { getX = x' - 1}
    move' (Arrow ARight) = c { getX = x' + 1}
    move' _ = c
