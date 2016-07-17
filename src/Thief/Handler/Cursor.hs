module Thief.Handler.Cursor
  ( Cursor(..)
  , defaultCursor
  , move
  )  where

import qualified Thief.Raw            as Raw

data Cursor = Cursor
  { x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  }
  deriving Show

defaultCursor :: Cursor
defaultCursor = Cursor 0 0 0 0

move :: Cursor -> Raw.Result -> Cursor
move c@(Cursor x' y' w h) = move'
  where
    move' (Raw.Arrow Raw.Up)    = Cursor x' (y' - 1) w h
    move' (Raw.Arrow Raw.Down)  = Cursor x' (y' + 1) w h
    move' (Raw.Arrow Raw.Left)  = Cursor (x' - 1) y' w h
    move' (Raw.Arrow Raw.Right) = Cursor (x' + 1) y' w h
    move' _ = c
