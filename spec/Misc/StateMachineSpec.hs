module Misc.StateMachineSpec where

import SpecHelper
import Control.Applicative (Alternative(..))
import Data.Char (toUpper)

import Misc.StateMachine

instance Eq a => Eq (StateMachine a) where
  Success a == Success b = a == b
  Pass a == Pass b = a == b
  Failure == Failure = True
  _ == _ = False

runString :: String -> StateMachine a -> Maybe a
runString (c:cs) cur = case runMore cur c of
    n@(More _) -> runString cs n
    Success a -> Just a
    Failure -> Nothing
    _ -> error "cannot be here"
runString [] _ = Nothing

spec :: Spec
spec = describe "StateMachine" $ do
  context "Basic" $
    it "parses single charactor" $ do
      let p = char 'x'
          f = runMore p 'y'
          s = runMore p 'x'
      f `shouldBe` Failure
      s `shouldBe` Success 'x'
  context "Functor" $
    it "is a functor" $ do
      let p = (:[]) <$> char 'x'
          f = runMore p 'y'
          s = runMore p 'x'
      f `shouldBe` Failure
      s `shouldBe` Success "x"
  context "Applicative" $ do
    it "process a string" $ do
      let f = runString "right" $ string "rirong"
          s = runString "fail" $ string "fail"
          s' = runString "abc123" $ string "abc1" <* string "23"
      f `shouldBe` Nothing
      s `shouldBe` Just "fail"
      s' `shouldBe` Just "abc1"
    it "process an integer" $ do
      let p = string "--{" *> integer <* string "}--"
          f = runString "--{64873}--" p
      f `shouldBe` Just 64873
  context "Alternative" $
    it "process a string" $ do
      let f = runString "hihiho" $ string "hellu" <|> string "hihihi"
          s = runString "hello" $ string "hello" <|> string "hihihi"
          l = runString "c" $ (toUpper <$> char 'c') <|> anyChar
      f `shouldBe` Nothing
      s `shouldBe` Just "hello"
      l `shouldBe` Just 'C'
