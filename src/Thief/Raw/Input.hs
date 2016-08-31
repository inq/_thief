module Thief.Raw.Input
  ( initialize
  , inputLoop
  ) where

import Control.Concurrent.Chan (Chan, writeChan)
import System.IO
  ( hSetBuffering, hSetEcho
  , stdin, stdout
  , BufferMode(NoBuffering))
import Thief.Raw.Result (Result)
import Thief.Status (Status, char)

initialize :: IO ()
initialize = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

inputLoop :: Chan Result -> Status -> IO ()
inputLoop c stat = do
    (next, res) <- char stat <$> getChar
    writeChan c res
    inputLoop c next
