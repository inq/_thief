module Thief.UI.Event
  ( Event(..)
  , fromRaw
  ) where

import qualified Thief.Raw.Event as Raw

data Event
  = Resize Int Int
  | Move Int Int
  | Char Char
  | Ctrl Char

fromRaw :: Raw.Event -> Event
fromRaw Raw.KeyUp = Move 0 (-1)
fromRaw Raw.KeyDown = Move 0 1
fromRaw Raw.KeyLeft = Move (-1) 0
fromRaw Raw.KeyRight = Move 1 0
fromRaw (Raw.Char '\ETB') = Ctrl 'w'
fromRaw (Raw.Char a) = Char a
fromRaw (Raw.Resize w h) = Resize w h
fromRaw _ = undefined
