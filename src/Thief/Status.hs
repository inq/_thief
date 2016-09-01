module Thief.Status
  ( Status
  , proceed
  , char
  ) where

import Misc (Default(def))
import qualified Thief.Raw.Result as Res
import Data.Char

-- * Data Constructors

data Status = Idle
  | Escape
  | Empty
  | Num Int
  | Chr Char
  | Pair Int Int
  | Trio Int Int Int
  deriving Show

instance Default Status where
  def = Idle

-- * Status

success :: Res.Result -> (Status, Res.Result)
-- ^ Finish the automata
success = (,) Idle

proceed :: Status -> (Status, Res.Result)
-- ^ Continue the automata
proceed s = (,) s Res.None

char :: Status -> Char -> (Status, Res.Result)
-- ^ The raw input automata
char Idle '\ESC' = proceed Escape
char Idle c
  | isLetter c = success $ Res.Char c
  | otherwise  = proceed $ Chr c
char Escape '[' = proceed Empty
char Empty c
  | isDigit c = proceed $ Num $ digitToInt c
  | c == 'A'  = success $ Res.Arrow Res.AUp
  | c == 'B'  = success $ Res.Arrow Res.ADown
  | c == 'C'  = success $ Res.Arrow Res.ARight
  | c == 'D'  = success $ Res.Arrow Res.ALeft
char (Num n) c
  | isDigit c = proceed $ Num (n * 10 + digitToInt c)
  | c == ';'  = proceed $ Pair n 0
char (Pair a b) c
  | isDigit c = proceed $ Pair a (b * 10 + digitToInt c)
  | c == ';'  = proceed $ Trio a b 0
  | c == 'R'  = success $ Res.Pair a b
char (Trio a b c) d
  | isDigit d = proceed $ Trio a b (c * 10 + digitToInt d)
  | d == 't'  = success $ Res.Trio a b c
char _ _ = (Idle, Res.None)
