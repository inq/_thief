module Thief.Term.Printable
  ( Printable (..)
  ) where

class Printable p where
  width :: p -> Int
  height :: p -> Int
