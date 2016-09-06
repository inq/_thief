module Misc.StateMachine
  ( StateMachine(..)
  , char
  ) where

data StateMachine a
  -- * More (Char -> (Result, Next))
  = More { runMore :: Char -> (Maybe a, StateMachine a) }
  -- * Func
  | Func a
  -- * No more actions
  | Success | Failure

instance Functor StateMachine where
  fmap f (More action) = More $ \c' ->
    let (re, ne) = action c'
    in  (f <$> re, f <$> ne)
  fmap f (Func a) = Func $ f a
  fmap _ Success = Success
  fmap _ Failure = Failure

char :: Char -> StateMachine Char
char c = More $ \c' -> if c == c'
  then (Just c, Success)
  else (Nothing, Failure)
