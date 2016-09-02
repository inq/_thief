module Thief.Handler.Status
  ( Status(..)
  ) where

import Misc (Default(def))
import Thief.Term.Cursor (Cursor)

-- * Data Constructors

data Status
  = Bare
    { getScreenSize :: Maybe (Int, Int)
    }
  | Ready
    { initialCursor :: (Int, Int)
    , getCursor :: Cursor
    }
  | Terminated

instance Default Status where
  def = Bare Nothing
