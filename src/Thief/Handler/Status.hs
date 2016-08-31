module Thief.Handler.Status
  ( Status(..)
  ) where

import Misc.Default (Default(..))
import Thief.Term.Cursor (Cursor(..))

data Status
  = Bare
    { getScreenSize :: Maybe (Int, Int)
    }
  | Ready
    { initialCursor :: (Int, Int)
    , getCursor :: Cursor
    }

instance Default Status where
  def = Bare Nothing
