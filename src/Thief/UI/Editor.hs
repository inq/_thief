module Thief.UI.Editor
  ( Editor(..)
  , initEditor
  ) where

import Misc (Default(def))
import Thief.UI.Common (Size(..), Drawable(..), Resizable(..))
import Thief.UI.Theme (Theme(editor))
import Thief.Term.Buffer (blankBuffer)

-- * Data Constructors

data Editor = MkEditor
  { getSize :: Size
  , getTheme :: Theme
  }

instance Drawable Editor where
  draw e@(MkEditor size theme) = initBuf
    where initBuf = blankBuffer (editor theme) (getWidth size) (getHeight size)

instance Resizable Editor where
  resize e size = e { getSize = size }

initEditor :: Theme -> Editor
initEditor = MkEditor undefined
