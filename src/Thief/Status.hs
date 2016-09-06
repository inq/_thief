module Thief.Status
  ( initialState
  ) where

import Misc (Default(def), StateMachine(..), string, char, integer, anyChar)
import Control.Applicative ((<|>))
import qualified Thief.Raw.Result as Res
import Data.Char

-- * Data Constructors

csi :: StateMachine String
csi = string "\ESC["

initialState :: StateMachine Res.Result
initialState
  =   Res.Arrow Res.AUp    <$ csi <* char 'A'
  <|> Res.Arrow Res.ADown  <$ csi <* char 'B'
  <|> Res.Arrow Res.ARight <$ csi <* char 'C'
  <|> Res.Arrow Res.ALeft  <$ csi <* char 'D'
  <|> Res.Pair <$ csi <*> integer <* char ';' <*> integer <* char 'R'
  <|> Res.Trio <$ csi <*> integer <* char ';' <*> integer <* char ';' <*> integer <* char 't'
  <|> Res.Char <$> anyChar
