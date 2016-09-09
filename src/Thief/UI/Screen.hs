module Thief.UI.Screen
  ( Screen(..)
  , initScreen
  ) where

import Misc (Default(def))
import Thief.UI.Common (Size(..), Drawable(..))
import Thief.UI.Window (Window)
import Thief.Term.Buffer (blankBuffer)
import Thief.Term.Brush (invertBrush)


-- * Data Constructors

data Screen = MkScreen
  { getSize :: Size
  , getWindows :: [Window]
  , getFocused :: Int
  }

instance Drawable Screen where
  draw s@(MkScreen size _ _) = initBuf
    where initBuf = blankBuffer (invertBrush def) (getWidth size) (getHeight size)

initScreen :: Size -> Screen
initScreen size = MkScreen size [] 0
