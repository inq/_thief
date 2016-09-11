module Thief.UI.Window
  ( Window(..)
  , initWindow
  ) where

import Misc (Default(def))
import Thief.UI.Common
  ( Size(MkSize)
  , Drawable(..)
  , Resizable(..)
  , Focusable(setFocus, releaseFocus)
  )
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

instance Resizable Window where
  resize win@MkWindow{ getEditor = editor } s@(MkSize w h) =
      win{ getSize = s, getEditor = editor' }
    where
      editor' = resize editor $ MkSize (w - 2) (h - 2)

instance Focusable Window where
  setFocus w = w{ getFocused = True }
  releaseFocus w = w{ getFocused = False }
