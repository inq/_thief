module Thief.UI.Window
  ( Window(..)
  , initWindow
  ) where

import Misc (Default(def))
import Thief.UI.Common
  ( Size(MkSize)
  , Drawable(..)
  , Editable(findCursor)
  , Focusable(setFocus, releaseFocus)
  , Responsable(event)
  , Coord(..)
  )
import Thief.Raw (Event(..))
import Thief.UI.Editor (Editor, initEditor)
import Thief.UI.Theme (Theme(..))
import Thief.Term.Buffer (blankBuffer, overlayBuffer)

-- * Data Constructors

data Window = MkWindow
  { getSize :: Size
  , getEditor :: Editor
  , getTheme :: Theme
  , getFocused :: Bool
  }

initWindow :: Theme -> Window
initWindow theme = MkWindow undefined (initEditor theme) theme False

instance Drawable Window where
  draw (MkWindow (MkSize w h) editor theme focused) = buf'
    where
      fillColor = if focused
        then windowFocused theme
        else windowUnFocused theme
      buf = blankBuffer fillColor w h
      buf' = overlayBuffer buf 1 1 $ draw editor

instance Editable Window where
  findCursor w = MkCoord (x + 1) (y + 1)
    where
      MkCoord x y = findCursor $ getEditor w

instance Responsable Window where
  event win@MkWindow{ getEditor = editor } (Resize w h) =
      win{ getSize = MkSize w h, getEditor = editor' }
    where
      editor' = event editor $ Resize (w - 2) (h - 2)

instance Focusable Window where
  setFocus w = w{ getFocused = True }
  releaseFocus w = w{ getFocused = False }
