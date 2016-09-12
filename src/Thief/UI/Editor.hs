module Thief.UI.Editor
  ( Editor(..)
  , initEditor
  ) where

import Misc (Default(def))
import Thief.UI.Common
  ( Size(..)
  , Coord(MkCoord)
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
  event ed = handle
    where
      MkEditor { getSize = MkSize w h, getCursor = MkCoord x y } = ed
      mkCoord x y = MkCoord (insideX x) (insideY y)
      insideX x
        | x < 0 = 0
        | x >= w = w - 1
        | otherwise = x
      insideY y
        | y < 0 = 0
        | y >= h = h - 1
        | otherwise = y
      handle (Resize w' h') = ed{ getSize = MkSize w' h' }
      handle KeyUp    = ed{ getCursor = mkCoord x (y - 1) }
      handle KeyDown  = ed{ getCursor = mkCoord x (y + 1) }
      handle KeyLeft  = ed{ getCursor = mkCoord (x - 1) y }
      handle KeyRight = ed{ getCursor = mkCoord (x + 1) y }
      handle (Char c) = ed

instance Editable Editor where
  findCursor = getCursor

initEditor :: Theme -> Editor
initEditor = MkEditor undefined def
