module Thief.Term
  ( Printable(..)
  -- * Cursor
  , Cursor(..)
  , moveCursor
  -- * Brush
  , Brush(..)
  , invertBrush
  -- * Buffer
  , Buffer(..)
  , borderedBuffer
  -- * Ansi
  , smcup
  , rmcup
  , movexy
  , moveCur
  , queryCursorPos
  ) where

import Thief.Term.Classes (Printable(..))
import Thief.Term.Cursor (Cursor(..), moveCursor)
import Thief.Term.Brush (Brush, invertBrush)
import Thief.Term.Buffer (Buffer, borderedBuffer)
import Thief.Term.Ansi (smcup, rmcup, movexy, moveCur, queryCursorPos)
