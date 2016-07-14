module Thief.Input (
  initialize,
  inputLoop
) where

import qualified System.IO          as IO
import qualified Control.Monad      as M
import qualified Thief.Status       as Stat
import qualified Thief.Ansi         as Ansi
import qualified Thief.Internal.FFI as FFI
import qualified Thief.Color        as Color

initialize :: IO ()
initialize = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetEcho IO.stdin False
    res <- FFI.getTermSize
    case res of
      Just (w, h) -> putStr $ Ansi.fillScreen w h Color.darkBlood
      Nothing -> putStrLn "== cannot inspect the terminal =="
    putStrLn $ show res
    putStrLn Ansi.queryCursorPos

inputLoop :: Stat.Status -> IO ()
inputLoop stat = do
    (next, res) <- Stat.char stat <$> getChar
    M.when (res /= Stat.None) $ putStrLn $ Stat.toStr res
    if res == Stat.RChar 'q'
      then return ()
      else inputLoop next
