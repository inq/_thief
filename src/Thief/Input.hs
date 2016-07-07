module Thief.Input where

import qualified System.IO     as IO
import qualified Control.Monad as M
import qualified Thief.Status  as Stat

initialize :: IO ()
initialize = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetEcho IO.stdin False
    putStr "\ESC[6n"

inputLoop :: Stat.Status -> IO ()
inputLoop stat = do
    (next, res) <- Stat.char stat <$> getChar
    M.when (res /= Stat.None) $ putStrLn $ Stat.toStr res
    inputLoop next
