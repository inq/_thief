module Thief.Input (
  initialize,
  inputLoop
) where

import qualified System.IO          as IO
import qualified Control.Monad      as M
import qualified Thief.Status       as Stat
import qualified Thief.Ansi         as Ansi

initialize :: IO ()
initialize = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetEcho IO.stdin False
    putStrLn Ansi.queryCursorPos

inputLoop :: Stat.Status -> IO ()
inputLoop (Stat.Status stat pos) = do
    (next, res) <- Stat.char stat <$> getChar
    if res == Stat.RChar 'q'
      then return ()
      else do
          let pos' = case res of
                Stat.RChar '↑' -> Stat.moveUp pos
                Stat.RChar '↓' -> Stat.moveDown pos
                Stat.RChar '→' -> Stat.moveRight pos
                Stat.RChar '←' -> Stat.moveLeft pos
                _ -> pos
          putStr $ Ansi.moveCursor pos'
          M.when (res /= Stat.None) $ putStr $ Stat.toStr res
          inputLoop (Stat.Status next pos')
