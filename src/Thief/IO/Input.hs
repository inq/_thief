module Thief.IO.Input (
  initialize,
  inputLoop
) where

import qualified System.IO          as IO
import qualified Control.Monad      as M
import qualified Thief.IO.Ansi      as Ansi
import qualified Thief.Status       as Stat
import qualified Thief.Color        as Color
import qualified Thief.Box          as Box

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
          M.when (res == Stat.RChar 'b') $ do
              putStr $ show theBox
          putStr $ Ansi.moveCursor pos'
          M.when (res /= Stat.None) $ putStr $ Stat.toStr res
          inputLoop (Stat.Status next pos')
  where
    borderColor = Color.lightGray
    fillColor = Color.darkBlue
    theBox = Box.Box 30 30 80 30 borderColor fillColor
