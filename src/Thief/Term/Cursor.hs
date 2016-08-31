module Thief.Term.Cursor
  ( Cursor (..)
  , move
  ) where

import Misc.Default (Default(..))
import Thief.Raw (Arrow(..), Result(..))
import Thief.Term.Brush (Brush)

data Cursor = MkCursor
  { theBrush :: Brush
  , theX :: Int
  , theY :: Int
  , theWidth :: Int
  , theHeight :: Int
  } deriving Show

instance Default Cursor where
  def = MkCursor def 0 0 0 0

move :: Cursor -> Result -> Cursor
move c@MkCursor { theX = x', theY = y' } = move'
  where
    move' (Arrow AUp)    = c { theY = y' - 1}
    move' (Arrow ADown)  = c { theY = y' + 1}
    move' (Arrow ALeft)  = c { theX = x' - 1}
    move' (Arrow ARight) = c { theX = x' + 1}
    move' _ = c
