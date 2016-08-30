module Thief.Term.LineSpec where

import SpecHelper

import Misc.Default (def)
import Thief.Term.Printable (width, height)
import Thief.Term.TChar (TChar(..))
import Thief.Term.Line


spec :: Spec
spec = describe "Line" $
  context "Printable" $ do
    it "calculates width & height" $ do
      let sc = MkChar def 'X'
          wc = MkChar def 'ㅡ'
      width (MkLine [sc, wc, sc, wc]) `shouldBe` 6
    it "produces left-aligned line" $
      width (leftAligned def 3 "Hello") `shouldBe` 3
