module Thief.Ansi where

queryCursorPos :: IO ()
queryCursorPos = do
    putStr "\ESC[6n"

queryScreenSize :: IO ()
queryScreenSize = do
    putStr "\ESC[19t"
