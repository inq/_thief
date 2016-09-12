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
      handle (Resize w' h') = (ed{ getSize = MkSize w' h' }, [Refresh])
      handle KeyUp    = ( ed{ getCursor = mkCoord x (y - 1) }
                        , [RMoveUp 1 | y > 0]
                        )
      handle KeyDown  = ( ed{ getCursor = mkCoord x (y + 1) }
                        , [RMoveDown 1 | y < h - 1]
                        )
      handle KeyLeft  = ( ed{ getCursor = mkCoord (x - 1) y }
                        , [RMoveLeft 1 | x > 0]
                        )
      handle KeyRight = ( ed{ getCursor = mkCoord (x + 1) y }
                        , [RMoveRight 1 | x < w - 1]
                        )
      handle (Char c) = ( ed
                        , [RChar c]
                        )

instance Editable Editor where
  findCursor = getCursor

initEditor :: Theme -> Editor
initEditor = MkEditor undefined def
