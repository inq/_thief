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
  , Result(..)
  )
import Thief.UI.Event (Event(..))
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
      handle (Resize w' h') = (ed{ getSize = MkSize w' h' }, [Refresh])
      handle (Move dx dy) =
          ( ed{ getCursor = MkCoord nx ny }
          , [RMove (nx - x) (ny - y)]
          )
        where
          nx = inside 0 (w - 1) (x + dx)
          ny = inside 0 (h - 1) (y + dy)
          inside min max num
            | num < min = min
            | num > max = max
            | otherwise = num
      handle (Char c) =
          ( ed
          , [RChar c]
          )

instance Editable Editor where
  findCursor = getCursor

initEditor :: Theme -> Editor
initEditor = MkEditor undefined def
