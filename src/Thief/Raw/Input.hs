module Thief.Raw.Input
  ( initialize
  , inputLoop
  ) where

import Control.Concurrent.Chan (Chan, writeChan)
import Control.Applicative ((<|>))
import System.IO
  ( hSetBuffering, hSetEcho
  , stdin, stdout
  , BufferMode(NoBuffering))
import Misc (StateMachine(..), string, char, integer, anyChar)
import Thief.Raw.Result (Result)
import qualified Thief.Raw.Result as Res

initialize :: IO ()
-- ^ Initialize the input
initialize = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

csi :: StateMachine String
csi = string "\ESC["

initialState :: StateMachine Res.Result
-- ^ The initial state of the StateMachine
initialState
  =   Res.Arrow Res.AUp    <$ csi <* char 'A'
  <|> Res.Arrow Res.ADown  <$ csi <* char 'B'
  <|> Res.Arrow Res.ARight <$ csi <* char 'C'
  <|> Res.Arrow Res.ALeft  <$ csi <* char 'D'
  <|> Res.Pair <$ csi <*> integer <* char ';' <*> integer <* char 'R'
  <|> Res.Trio <$ csi <*> integer <* char ';' <*> integer <* char ';' <*> integer <* char 't'
  <|> Res.Char <$> anyChar

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
