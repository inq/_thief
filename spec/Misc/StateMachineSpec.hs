module Misc.StateMachineSpec where

import SpecHelper

import Misc.StateMachine

spec :: Spec
spec = describe "StateMachine" $ do
  context "Basic" $ do
    it "parses single charactor" $ do
      let p = char 'x'
          (f, _) = (runMore p) 'y'
          (s, _) = (runMore p) 'x'
      f `shouldBe` Nothing
      s `shouldBe` Just 'x'
  context "Functor" $ do
    it "is a functor" $ do
      let p = (:[]) <$> char 'x'
          (f, _) = (runMore p) 'y'
          (s, _) = (runMore p) 'x'
      f `shouldBe` Nothing
      s `shouldBe` Just "x"
