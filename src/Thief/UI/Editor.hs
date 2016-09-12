module Thief.UI.Editor
  ( Editor(..)
  , initEditor
  ) where

import Misc (Default(def))
import Thief.UI.Common
  ( Size(..)
  , Coord
  , Drawable(..)
  , Responsable(event)
  , Editable(findCursor)
  )
import Thief.Raw (Event(..))
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

instance Responsable Editor where
  event e (Resize w h) = e { getSize = MkSize w h }

instance Editable Editor where
  findCursor = getCursor

initEditor :: Theme -> Editor
initEditor = MkEditor undefined def
