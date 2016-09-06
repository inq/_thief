module Misc.StateMachineSpec where

import SpecHelper

import Misc.StateMachine

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
      let p0 = string "hello"
          (_, p1) = runMore p0 'h'
          (_, p2) = runMore p1 'e'
          (_, p3) = runMore p2 'l'
          (_, p4) = runMore p3 'l'
          (r5, _) = runMore p4 'o'
          (r6, _) = runMore p2 'h'
      r5 `shouldBe` Just "hello"
      r6 `shouldBe` Nothing
