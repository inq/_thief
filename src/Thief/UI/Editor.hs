module Thief.UI.Editor
  ( Editor(..)
  , initEditor
  ) where

import Misc (Default(def))
import Thief.UI.Common
  ( Size(..)
  , Coord
  , Drawable(..)
  , Resizable(..)
  , Editable(findCursor)
  )
import Thief.UI.Theme (Theme(editor))
import Thief.Term.Buffer (blankBuffer)

-- * Data Constructors

data Editor = MkEditor
  { getSize :: Size
  , getCursor :: Coord
  , getTheme :: Theme
  }

instance Drawable Editor where
  draw e@(MkEditor size _ theme) = initBuf
    where initBuf = blankBuffer (editor theme) (getWidth size) (getHeight size)

instance Resizable Editor where
  resize e size = e { getSize = size }

instance Editable Editor where
  findCursor = getCursor

initEditor :: Theme -> Editor
initEditor = MkEditor undefined def
