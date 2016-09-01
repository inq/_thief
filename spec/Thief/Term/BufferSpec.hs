module Thief.Term.BufferSpec where

import SpecHelper

import Misc (def)
import Thief.Term.Brush (Brush(..))
import Thief.Term.Classes (Printable(..))
import Thief.Term.Line (Line(..))
import Thief.Term.TChar (TChar(..))
import Thief.Term.Buffer


spec :: Spec
spec = describe "Buffer" $
  context "Printable" $
    it "calculates width & height" $ do
      let sc = MkChar def 'X'
          wc = MkChar def 'ㅡ'
      width (MkBuffer [MkLine [sc, wc, sc, wc]]) `shouldBe` 6
