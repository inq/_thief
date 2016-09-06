module Thief.Raw.Input
  ( initialize
  , inputLoop
  ) where

import Control.Concurrent.Chan (Chan, writeChan)
import System.IO
  ( hSetBuffering, hSetEcho
  , stdin, stdout
  , BufferMode(NoBuffering))
import Misc (StateMachine(..))
import Thief.Raw.Result (Result)
import Thief.Status (initialState)

initialize :: IO ()
-- ^ Initialize the input
initialize = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

inputLoop :: Chan Result -> IO ()
-- ^ Input from the terminal
inputLoop c = inputLoop' initialState
  where
    inputLoop' stat = do
      next <- runMore stat <$> getChar
      case next of
        More _ -> inputLoop' next
        Success a -> writeChan c a >> inputLoop' initialState
        _ -> inputLoop' initialState
