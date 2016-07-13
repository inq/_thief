module Thief.Input (
  initialize,
  inputLoop
) where

import qualified System.IO          as IO
import qualified Control.Monad      as M
import qualified Thief.Status       as Stat
import qualified Thief.Ansi         as Ansi
import qualified Thief.Internal.FFI as FFI

initialize :: IO ()
initialize = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetEcho IO.stdin False
    res <- FFI.getTermSize
    putStrLn $ show res
    Ansi.queryCursorPos

inputLoop :: Stat.Status -> IO ()
inputLoop stat = do
    (next, res) <- Stat.char stat <$> getChar
    M.when (res /= Stat.None) $ putStrLn $ Stat.toStr res
    if res == Stat.RChar 'q'
      then return ()
      else inputLoop next
