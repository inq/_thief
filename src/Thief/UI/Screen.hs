module Thief.UI.Screen
  ( Screen(..)
  , initScreen
  ) where

import Misc (Default(def))
import Thief.UI.Common (Size(MkSize), Drawable(..), Resizable(..))
import Thief.UI.Window (Window(MkWindow), initWindow)
import Thief.UI.Theme (Theme(..))
import Thief.Term.Buffer (blankBuffer, overlayBuffer)
import Thief.Term.Brush (invertBrush)

-- * Data Constructors

data Screen = MkScreen
  { getSize :: Size
  , getWindows :: [Window]
  , getFocused :: Int
  , getTheme :: Theme
  }

instance Drawable Screen where
  draw s@(MkScreen size [w1, w2] _ _) = buf''
    where
      MkSize w h = size
      buf = blankBuffer (invertBrush def) w h
      buf' = overlayBuffer buf 1 1 $ draw w1
      buf'' = overlayBuffer buf' (w `div` 2 + 1) 1 $ draw w2

instance Resizable Screen where
  resize (scr@MkScreen{ getWindows = [w1, w2] }) s@(MkSize w h) =
      scr{ getSize = s, getWindows = [w1', w2'] }
    where
      w1' = resize w1 $ MkSize ((w - 3 + 1) `div` 2) (h - 2)
      w2' = resize w2 $ MkSize ((w - 3) `div` 2) (h - 2)

initScreen :: Theme -> Size -> Screen
initScreen theme = resize $ MkScreen undefined windows 0 theme
  where
    windows =
      [ initWindow theme
      , initWindow theme
      ]
