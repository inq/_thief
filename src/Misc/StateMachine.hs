module Misc.StateMachine
  ( StateMachine(..)
  , char
  , string
  ) where

import Control.Applicative (Alternative(..))

data StateMachine a
  -- * More (Char -> (Result, Next))
  = More { runMore :: Char -> (Maybe a, StateMachine a) }
  -- * Func
  | Func a
  -- * No more actions
  | Success | Failure

instance Functor StateMachine where
  fmap f (More action) = More $ \c' ->
    let (result, next) = action c'
    in  (f <$> result, f <$> next)
  fmap f (Func a) = Func $ f a
  fmap _ Success = Success
  fmap _ Failure = Failure

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
    let Success ||| Just x = pure x
        a       ||| _      = a
        (resultA, nextA) = actionA c'
    in  (Nothing, (nextA ||| resultA) <*> More actionB)
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

char :: Char -> StateMachine Char
char c = More $ \c' -> if c == c'
  then (Just c, Success)
  else (Nothing, Failure)

string :: String -> StateMachine String
string (c:cs) = pure (:) <*> char c <*> string cs
string [] = pure []
