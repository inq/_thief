module Thief.UI.Window
  ( Window(..)
  , initWindow
  ) where

import Misc (Default(def))
import Thief.UI.Common (Size(MkSize), Drawable(..), Resizable(..))
import Thief.UI.Editor (Editor, initEditor)
import Thief.UI.Theme (Theme(..))
import Thief.Term.Buffer (blankBuffer, overlayBuffer)

-- * Data Constructors

data Window = MkWindow
  { getSize :: Size
  , getEditor :: Editor
  , getTheme :: Theme
  }

initWindow :: Theme -> Window
initWindow theme = MkWindow undefined (initEditor theme) theme

instance Drawable Window where
  draw (MkWindow (MkSize w h) editor theme) = buf'
    where
      buf = blankBuffer def w h
      buf' = overlayBuffer buf 1 1 $ draw editor

instance Resizable Window where
  resize win@MkWindow{ getEditor = editor } s@(MkSize w h) =
      win{ getSize = s, getEditor = editor' }
    where
      editor' = resize editor $ MkSize (w - 2) (h - 2)
