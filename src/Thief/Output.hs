module Thief.Output where

initialize :: IO ()
initialize = do
    putStr "\ESC[?47h" -- smcup

finalize :: IO ()
finalize = do
    putStr "\ESC[?47l" -- rmcup
