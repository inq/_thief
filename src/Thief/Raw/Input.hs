module Thief.Raw.Input
  ( initialize
  , inputLoop
  ) where

import qualified Control.Concurrent.Chan as C
import qualified System.IO          as IO
import qualified Thief.Raw.Result   as Res
import qualified Thief.Status       as Stat

initialize :: IO ()
initialize = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetEcho IO.stdin False

inputLoop :: C.Chan Res.Result -> Stat.Status -> IO ()
inputLoop c stat = do
    (next, res) <- Stat.char stat <$> getChar
    C.writeChan c res
    inputLoop c next
