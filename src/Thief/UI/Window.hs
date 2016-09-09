module Thief.UI.Window
  ( Window(..)
  ) where

import Thief.UI.Common (Size)

-- * Data Constructors

data Window = MkWindow
  { getSize :: Size
  }
