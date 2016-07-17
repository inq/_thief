module Thief.IO.Input (
  initialize,
  inputLoop
) where

import qualified System.IO          as IO
import qualified Control.Monad      as M
import qualified Thief.IO.Ansi      as Ansi
import qualified Thief.IO.Result    as Res
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
    if res == Res.Char 'q'
      then return ()
      else do
          let pos' = case res of
                Res.Arrow Res.Up    -> Stat.moveUp pos
                Res.Arrow Res.Down  -> Stat.moveDown pos
                Res.Arrow Res.Right -> Stat.moveRight pos
                Res.Arrow Res.Left  -> Stat.moveLeft pos
                _ -> pos
          M.when (res == Res.Char 'b') $ do
              putStr $ show theBox
          putStr $ Ansi.moveCursor pos'
          M.when (res /= Res.None) $ putStr $ Stat.toStr res
          inputLoop (Stat.Status next pos')
  where
    borderColor = Color.lightGray
    fillColor = Color.darkBlue
    theBox = Box.Box 30 30 80 30 borderColor fillColor
