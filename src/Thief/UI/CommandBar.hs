module Thief.UI.CommandBar
  ( CommandBar(..)
  , initCommandBar
  ) where

import Thief.UI.Event (Event(..))
import Thief.UI.Common
  ( Drawable(draw)
  , Responsable(event)
  , Result(Refresh)
  )
import Thief.UI.Theme (Theme(commandBar, commandBarHead))
import Thief.Term.Line (blankLine, overlayLine, fromString)
import Thief.Term.Buffer (Buffer(MkBuffer))

-- * Data Constructors

data CommandBar = MkCommandBar
  { getTheme :: Theme
  , getWidth :: Int
  }

instance Drawable CommandBar where
  draw c = buf
    where
      MkCommandBar{ getTheme = t, getWidth = w } = c
      l = blankLine (commandBar t) w
      l' = overlayLine l 0 $ fromString (commandBarHead t) " : "
      buf = MkBuffer [l']

instance Responsable CommandBar where
  event c@MkCommandBar
      { getWidth = w
      } = handle
    where
      handle (Resize w h) =
        ( c{ getWidth = w }
        , [Refresh]
        )

initCommandBar :: Theme -> CommandBar
initCommandBar t = MkCommandBar t undefined
