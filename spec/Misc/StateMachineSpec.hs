module Misc.StateMachineSpec where

import SpecHelper
import Control.Applicative (Alternative(..))

import Misc.StateMachine

runString :: String -> StateMachine String -> Maybe String
runString (c:cs) cur = case next of
    Failure -> res
    More _ -> runString cs next
    Success -> res
    _ -> error "cannot be here"
  where
    (res, next) = runMore cur c
runString [] _ = Nothing

spec :: Spec
spec = describe "StateMachine" $ do
  context "Basic" $ do
    it "parses single charactor" $ do
      let p = char 'x'
          (f, _) = runMore p 'y'
          (s, _) = runMore p 'x'
      f `shouldBe` Nothing
      s `shouldBe` Just 'x'
  context "Functor" $ do
    it "is a functor" $ do
      let p = (:[]) <$> char 'x'
          (f, _) = runMore p 'y'
          (s, _) = runMore p 'x'
      f `shouldBe` Nothing
      s `shouldBe` Just "x"
  context "Applicative" $ do
    it "process a string" $ do
      let f = runString "hello" $ string "hellu"
          s = runString "hello" $ string "hello"
      f `shouldBe` Nothing
      s `shouldBe` Just "hello"
  context "Alternative" $ do
    it "process a string" $ do
      let f = runString "hihiho" $ string "hellu" <|> string "hihihi"
          s = runString "hello" $ string "hello" <|> string "hihihi"
      f `shouldBe` Nothing
      s `shouldBe` Just "hello"
