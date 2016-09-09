module Thief.UI.Screen
  ( Screen(..)
  , initScreen
  ) where

import Misc (Default(def))
import Thief.UI.Common (Size(..), Drawable(..), Resizable(..))
import Thief.UI.Window (Window(MkWindow))
import Thief.Term.Buffer (blankBuffer, overlayBuffer)
import Thief.Term.Brush (invertBrush)

-- * Data Constructors

data Screen = MkScreen
  { getSize :: Size
  , getWindows :: [Window]
  , getFocused :: Int
  }

instance Drawable Screen where
  draw s@(MkScreen size [w1, w2] _) = buf''
    where
      MkSize w h = size
      buf = blankBuffer (invertBrush def) w h
      buf' = overlayBuffer buf 1 1 $ draw w1
      buf'' = overlayBuffer buf' (w `div` 2 + 1) 1 $ draw w2

instance Resizable Screen where
  resize (scr@MkScreen{ getWindows = [w1, w2] }) size = scr{ getSize = size, getWindows = [w1', w2'] }
    where
      MkSize w h = size
      w1' = resize w1 $ MkSize ((w - 3 + 1) `div` 2) (h - 2)
      w2' = resize w2 $ MkSize ((w - 3) `div` 2) (h - 2)

initScreen :: Size -> Screen
initScreen = resize $ MkScreen undefined [MkWindow undefined, MkWindow undefined] 0
