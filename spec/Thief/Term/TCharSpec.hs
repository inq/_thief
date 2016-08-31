module Thief.Term.TCharSpec where

import SpecHelper

import Misc.Default (def)
import Thief.Term.Brush (invert)
import Thief.Term.Classes (Printable(..))
import Thief.Term.TChar


spec :: Spec
spec = describe "TChar" $
  context "Printable" $ do
    it "calculates width & height" $ do
      let sc = MkChar def 'X'
          wc = MkChar def 'ㅡ'
      width sc `shouldBe` 1
      width wc `shouldBe` 2
      height sc `shouldBe` 1
      height wc `shouldBe` 1
    it "generate ansi string" $ do
      let sc = MkChar def '-'
      toAnsi (invert def) sc
          `shouldBe` (def, "\ESC[38;2;200;200;200m\ESC[48;2;50;50;50m-")
      toAnsi def sc
          `shouldBe` (def, "-")
