module Thief.UI.Screen
  ( Screen(..)
  , initScreen
  , drawScreen
  , diffScreen
  ) where

import Misc (Default(def))
import Thief.UI.Event (Event(..))
import Thief.UI.Common
  ( Size(MkSize)
  , Coord(MkCoord)
  , Drawable(..)
  , Editable(findCursor)
  , Responsable(event)
  , Result(Refresh)
  , Focusable(setFocus, releaseFocus)
  )
import Thief.UI.CommandBar (CommandBar, initCommandBar)
import Thief.UI.Window (Window(MkWindow), initWindow)
import Thief.UI.Theme (Theme(..))
import Thief.Term.Buffer (blankBuffer, overlayBuffer)
import Thief.Term.Brush (invertBrush)
import Thief.Term
  ( Printable(toAnsi)
  , Cursor(theX, theY, theWidth, theHeight), moveCursor
  , invertBrush
  , borderedBuffer
  , civis, cvvis, movexy, moveCur
  )

-- * Data Constructors

data Screen = MkScreen
  { getSize :: Size
  , getWindows :: [(Coord, Window)]
  , getCommandBar :: (Coord, CommandBar)
  , getFocused :: Int
  , getTheme :: Theme
  }

instance Drawable Screen where
  draw scr = buf'''
    where
      MkScreen
        { getSize = size
        , getWindows = [(c1, w1), (c2, w2)]
        , getCommandBar = (c3, b3)
        } = scr
      MkSize w h = size
      MkCoord x1 y1 = c1
      MkCoord x2 y2 = c2
      MkCoord x3 y3 = c3
      buf = blankBuffer (invertBrush def) w h
      buf' = overlayBuffer buf x1 y1 $ draw w1
      buf'' = overlayBuffer buf' x2 y2 $ draw w2
      buf''' = overlayBuffer buf'' x3 y3 $ draw b3

instance Editable Screen where
  findCursor scr = MkCoord (x + x') (y + y')
    where
      MkScreen{ getWindows = ws, getFocused = f } = scr
      (c, w) = ws !! f
      MkCoord x' y' = c
      MkCoord x y = findCursor w

instance Responsable Screen where
  event scr@MkScreen
      { getSize = size
      , getWindows = ws@[(c1, w1), (c2, w2)]
      , getCommandBar = (c3, w3)
      , getFocused = i
      } = handle
    where
      handle (Resize w h) =
          ( scr
            { getSize = MkSize w h
            , getWindows = [(c1', w1'), (c2', w2')]
            , getCommandBar = (c3', w3')
            }
          , [Refresh]
          )
        where
          c1' = MkCoord 1 1
          c2' = MkCoord (w `div` 2 + 1) 1
          c3' = MkCoord 0 (h - 1)
          (w1', r1) = event w1 $ Resize ((w - 3 + 1) `div` 2) (h - 2)
          (w2', r2) = event w2 $ Resize ((w - 3) `div` 2) (h - 2)
          (w3', r3) = event w3 $ Resize w 1
      handle (Ctrl 'w') =
          ( rotateFocus scr
          , [Refresh]
          )
      handle e =
          ( scr{ getWindows = ws' }
          , res
          )
        where
          (res, ws') = eventFocused i ws
          eventFocused i ((c, w) : ws)
            | i == 0 = (res', (c, w') : ws)
            | otherwise = append' (c, w) (eventFocused (i - 1) ws)
            where
              (w', res') = event w e
              append' a (b, c) = (b, a : c)
          eventFocused _ _ = ([], [])

drawScreen :: Screen -> String
drawScreen scr = concat
    [ civis
    , movexy 0 0
    , snd $ toAnsi def $ draw scr
    , cvvis
    ,movexy (x + 1) (y + 1)
    ]
  where MkCoord x y = findCursor scr

diffScreen :: Screen -> Screen -> String
diffScreen p c = concat $ diff ++ [ movexy (x + 1) (y + 1) ]
  where
    diff = if ps == cs
      then []
      else
        [ civis, movexy 0 0, cs, cvvis ]
    ps = snd $ toAnsi def $ draw p
    cs = snd $ toAnsi def $ draw c
    MkCoord x y = findCursor c

initScreen :: Theme -> Screen
initScreen theme = MkScreen undefined windows cb 0 theme
  where
    windows =
      [ (undefined, setFocus $ initWindow theme)
      , (undefined, initWindow theme)
      ]
    cb = (undefined, initCommandBar theme)

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
