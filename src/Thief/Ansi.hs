module Thief.Ansi where

queryCursorPos :: IO ()
queryCursorPos = do
    putStr "\ESC[6n"
