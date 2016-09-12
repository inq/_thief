module Thief.Handler.Status
  ( Status(..)
  ) where

import Misc (Default(def))
import Thief.Term.Cursor (Cursor)
import Thief.UI.Screen (Screen)

-- * Data Constructors

data Status
  = Bare
    { getScreenSize :: Maybe (Int, Int)
    }
  | Ready
    { initialCursor :: (Int, Int)
    , getScreen :: Screen
    }
  | Terminated

instance Default Status where
  def = Bare Nothing
