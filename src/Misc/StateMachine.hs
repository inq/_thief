module Misc.StateMachine
  ( StateMachine(..)
  , char
  , anyChar
  , string
  , integer
  ) where

import Control.Applicative (Alternative(..))
import Data.Char (isDigit, digitToInt)
import Debug.Trace (trace)

-- * Data Constructors

data StateMachine a
  -- * More (Char -> (Result, Next))
  = More { runMore :: Char -> StateMachine a }
  -- * No more actions
  | Success a | Pass a | Failure

instance Show (StateMachine a) where
  show (More _) = "More *"
  show (Success _) = "Success *"
  show (Pass _) = "Pass *"
  show Failure = "Failure"

instance Functor StateMachine where
  fmap f (More action) = More $ \c' -> f <$> action c'
  fmap f (Success a) = Success $ f a
  fmap f (Pass a) = Pass $ f a
  fmap _ Failure = Failure

instance Applicative StateMachine where
  pure = Success
  Success a <*> Success b = Success $ a b
  Success a <*> More actionB = More $ \c' -> a <$> actionB c'
  Success a <*> Pass b = Pass (a b)
  More actionA <*> Success b = More $ \c' -> actionA c' <*> pure b
  More actionA <*> More actionB = More $ \c' -> case actionA c' of
      Pass x -> pure x <*> actionB c'
      a      -> a <*> More actionB
  _ <*> _ = Failure

instance Alternative StateMachine where
  empty = Failure
  More actionA <|> More actionB = More $ \c' -> actionA c' <|> actionB c'
  More actionA <|> _ = More actionA
  Success a <|> _ = Success a
  _ <|> a = a

-- * StateMachine

char :: Char -> StateMachine Char
-- ^ Accept a single character
char c = More $ \c' -> if c == c' then Success c' else Failure

anyChar :: StateMachine Char
-- ^ Accept any character
anyChar = More $ \c' -> Success c'

string :: String -> StateMachine String
-- ^ Accept a sequential string
string = foldr (\c n -> (:) <$> char c <*> n) (pure [])

integer :: StateMachine Int
-- ^ Accept an integer
integer = integer' 0
  where
    integer' acc = More $ \c' -> if isDigit c'
      then integer' $ acc * 10 + digitToInt c'
      else Pass acc
