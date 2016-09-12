{-# LANGUAGE ForeignFunctionInterface #-}

module Thief.Raw.FFI
  ( getTermSize
  ) where

import Foreign.C.Types (CInt(..), CUShort(..))
import Foreign

#include <sys/ioctl.h>
#include <unistd.h>

-- * Data Constructors

data Winsize = Winsize
  { wsRow :: CUShort
  , wsCol :: CUShort
  , wsXpixel :: CUShort
  , wsYpixel :: CUShort }

foreign import ccall "sys/ioctl.h ioctl"
    ioctl :: CInt -> CInt -> Ptr Winsize -> IO CInt

instance Storable Winsize where
    sizeOf _ = (#size struct winsize)
    alignment _ = (#alignment struct winsize)
    peek ptr = do
        r <- (#peek struct winsize, ws_row) ptr
        c <- (#peek struct winsize, ws_col) ptr
        x <- (#peek struct winsize, ws_xpixel) ptr
        y <- (#peek struct winsize, ws_ypixel) ptr
        return $ Winsize r c x y
    poke ptr (Winsize r c x y) = do
        (#poke struct winsize, ws_row) ptr r
        (#poke struct winsize, ws_col) ptr c
        (#poke struct winsize, ws_xpixel) ptr x
        (#poke struct winsize, ws_ypixel) ptr y

getTermSize :: IO (Int, Int)
-- ^ Get the terminal size
getTermSize =
    with (Winsize 0 0 0 0) $ \ws -> do
        res <- ioctl (#const STDOUT_FILENO) (#const TIOCGWINSZ) ws
        if res < 0
            then error "getTermSize Error"
            else do
              Winsize col row _ _ <- peek ws
              return (fromIntegral row, fromIntegral col)
