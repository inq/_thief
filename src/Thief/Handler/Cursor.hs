module Thief.Handler.Cursor
  ( Cursor(..)
  , defaultCursor
  , moveUp
  , moveDown
  , moveLeft
  , moveRight
  )  where

data Cursor = Cursor
  { x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  }
  deriving Show

defaultCursor :: Cursor
defaultCursor = Cursor 0 0 0 0

moveUp :: Cursor -> Cursor
moveUp (Cursor x' y' w h) = Cursor x' (y' - 1) w h

moveDown :: Cursor -> Cursor
moveDown (Cursor x' y' w h) = Cursor x' (y' + 1) w h

moveLeft :: Cursor -> Cursor
moveLeft (Cursor x' y' w h) = Cursor (x' - 1) y' w h

moveRight :: Cursor -> Cursor
moveRight (Cursor x' y' w h) = Cursor (x' + 1) y' w h
