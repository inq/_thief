module Thief.Term.TCharSpec where

import SpecHelper

import Misc.Default (def)
import Thief.Term.Printable (width, height)
import Thief.Term.TChar


spec :: Spec
spec = describe "TChar" $
  context "Printable" $
    it "calculates width & height" $ do
      let sc = MkChar def 'X'
          wc = MkChar def 'ㅡ'
      width sc `shouldBe` 1
      width wc `shouldBe` 2
      height sc `shouldBe` 1
      height wc `shouldBe` 1
