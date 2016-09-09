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
import Thief.Raw.Event (Event(..))

initialize :: IO ()
-- ^ Initialize the input
initialize = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

initialState :: StateMachine Event
-- ^ The initial state of the StateMachine
initialState
    =   KeyUp    <$ csi <* char 'A'
    <|> KeyDown  <$ csi <* char 'B'
    <|> KeyRight <$ csi <* char 'C'
    <|> KeyLeft  <$ csi <* char 'D'
    <|> Pair     <$ csi <*> integer <* char ';' <*> integer <* char 'R'
    <|> Trio     <$ csi <*> integer <* char ';' <*> integer <* char ';' <*> integer <* char 't'
    <|> Char     <$> anyChar
  where
    csi = string "\ESC["

inputLoop :: Chan Event -> IO ()
-- ^ Input from the terminal
inputLoop c = inputLoop' initialState
  where
    inputLoop' stat = do
      next <- runMore stat <$> getChar
      case next of
        More _ -> inputLoop' next
        Success a -> writeChan c a >> inputLoop' initialState
        _ -> inputLoop' initialState
