module Thief.Input where

import qualified System.IO    as IO
import qualified Thief.Status as Stat
import Data.Char

initialize :: IO ()
initialize = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetEcho IO.stdin False
    putStr "\ESC[6n"

inputLoop :: Stat.Status -> IO ()
inputLoop stat = do
    res <- Stat.char stat <$> getChar
    let next = case res of
          Just x -> x
          _      -> Stat.Idle
    putStrLn $ show next
    inputLoop next
