module Thief.Input where

import qualified System.IO as IO

initialize :: IO ()
initialize = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetEcho IO.stdin False

inputLoop :: IO ()
inputLoop = do
     c <- getChar
     putChar c
