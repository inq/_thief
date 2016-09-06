module Misc.StateMachine
  ( StateMachine(..)
  , char
  , string
  , integer
  ) where

import Control.Applicative (Alternative(..))
import Data.Char (isDigit, digitToInt)

-- * Data Constructors

data StateMachine a
  -- * More (Char -> (Result, Next))
  = More { runMore :: Char -> (Maybe a, StateMachine a) }
  -- * Func
  | Func a
  -- * No more actions
  | Success | Failure | Pass

instance Show (StateMachine a) where
  show (More _) = "More *"
  show (Func _) = "Func *"
  show (Success) = "Success"
  show (Failure) = "Failure"
  show (Pass) = "Pass"

instance Functor StateMachine where
  fmap f (More action) = More $ \c' ->
    let (result, next) = action c'
    in  (f <$> result, f <$> next)
  fmap f (Func a) = Func $ f a
  fmap _ Success = Success
  fmap _ Failure = Failure
  fmap _ Pass = Pass

instance Applicative StateMachine where
  pure f = Func f
  Func a <*> Func b = Func (a b)
  Func a <*> More actionB = More $ \c' ->
    let (resultB, nextB) = actionB c'
    in  (a <$> resultB, a <$> nextB)
  More actionA <*> Func b = More $ \c' ->
    let (resultA, nextA) = actionA c'
    in  (resultA <*> pure b, nextA <*> pure b)
  More actionA <*> More actionB = More $ \c' ->
    let (resultB, nextB) = actionB c'
    in case actionA c' of
      (Just x, Success) -> (Nothing, pure x <*> More actionB)
      (Just x, Pass)         -> (Just x <*> resultB, pure x <*> nextB)
      (_, a)            -> (Nothing, a <*> More actionB)
  Failure <*> _ = Failure
  _ <*> Failure = Failure
  _ <*> _ = Success

instance Alternative StateMachine where
  empty = Failure
  More actionA <|> More actionB = More $ \c' ->
    let (resultA, nextA) = actionA c'
        (resultB, nextB) = actionB c'
    in  (resultA <|> resultB, nextA <|> nextB)
  Failure <|> a = a
  a <|> Failure = a
  _ <|> _ = Failure

-- * StateMachine

char :: Char -> StateMachine Char
-- ^ Accept a single character
char c = More $ \c' -> if c == c'
  then (Just c, Success)
  else (Nothing, Failure)

string :: String -> StateMachine String
-- ^ Accept a sequential string
string (c:cs) = pure (:) <*> char c <*> string cs
string [] = pure []

integer :: StateMachine Int
-- ^ Accept an integer
integer = integer' 0
  where
    integer' acc = More $ \c' -> if isDigit c'
      then (Nothing, integer' $ acc * 10 + digitToInt c')
      else (Just acc, Pass)
