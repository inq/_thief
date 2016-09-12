module Thief.UI.Screen
  ( Screen(..)
  , initScreen
  , rotateFocus
  ) where

import Misc (Default(def))
import Thief.UI.Common
  ( Size(MkSize)
  , Coord(MkCoord)
  , Drawable(..)
  , Editable(findCursor)
  , Resizable(..)
  , Focusable(setFocus, releaseFocus)
  )
import Thief.UI.Window (Window(MkWindow), initWindow)
import Thief.UI.Theme (Theme(..))
import Thief.Term.Buffer (blankBuffer, overlayBuffer)
import Thief.Term.Brush (invertBrush)

-- * Data Constructors

data Screen = MkScreen
  { getSize :: Size
  , getWindows :: [(Coord, Window)]
  , getFocused :: Int
  , getTheme :: Theme
  }

instance Drawable Screen where
  draw scr = buf''
    where
      MkScreen{ getSize = size, getWindows = [(c1, w1), (c2, w2)] } = scr
      MkSize w h = size
      MkCoord x1 y1 = c1
      MkCoord x2 y2 = c2
      buf = blankBuffer (invertBrush def) w h
      buf' = overlayBuffer buf x1 y1 $ draw w1
      buf'' = overlayBuffer buf' x2 y2 $ draw w2

instance Editable Screen where
  findCursor scr = MkCoord (x + x') (y + y')
    where
      MkScreen{ getWindows = ws, getFocused = f } = scr
      (c, w) = ws !! f
      MkCoord x' y' = c
      MkCoord x y = findCursor w

instance Resizable Screen where
  resize scr s =
      scr{ getSize = s, getWindows = [(c1', w1'), (c2', w2')] }
    where
      MkScreen{ getWindows = [(c1, w1), (c2, w2)] } = scr
      MkSize w h = s
      c1' = MkCoord 1 1
      c2' = MkCoord (w `div` 2 + 1) 1
      w1' = resize w1 $ MkSize ((w - 3 + 1) `div` 2) (h - 2)
      w2' = resize w2 $ MkSize ((w - 3) `div` 2) (h - 2)

initScreen :: Theme -> Size -> Screen
initScreen theme = resize $ MkScreen undefined windows 0 theme
  where
    windows =
      [ (undefined, setFocus $ initWindow theme)
      , (undefined, initWindow theme)
      ]

rotateFocus :: Screen -> Screen
rotateFocus s@MkScreen{ getWindows = ws, getFocused = i } =
    s{ getWindows = replace i i' ws, getFocused = i' }
  where
    i' = (i + 1) `mod` length ws
    replace u s ((c, a):as) = conv (c, a) : replace (u - 1) (s - 1) as
      where
        conv (c, a)
          | u == 0 = (c, releaseFocus a)
          | s == 0 = (c, setFocus a)
          | otherwise = (c, a)
    replace _ _ [] = []
