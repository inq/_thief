{-# LANGUAGE ForeignFunctionInterface #-}

module Thief.Term.FFI
  ( wcWidth
  ) where

import Foreign
import Foreign.C.Types (CWchar(..), CInt(..))

#include <wchar.h>

foreign import ccall "wchar.h wcwidth"
    wcwidth :: CWchar -> CInt

wcWidth :: Char -> Int
-- ^ Get the width of the charactor
wcWidth = fromEnum . wcwidth . toEnum . fromEnum
