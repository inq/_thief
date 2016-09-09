module Thief.UI.Window
  ( Window(..)
  ) where

import Misc (Default(def))
import Thief.UI.Common (Size(..), Drawable(..), Resizable(..))
import Thief.Term.Buffer (blankBuffer)

-- * Data Constructors

data Window = MkWindow
  { getSize :: Size
  }

instance Drawable Window where
  draw w@(MkWindow size) = initBuf
    where initBuf = blankBuffer def (getWidth size) (getHeight size)

instance Resizable Window where
  resize w size = w { getSize = size }
