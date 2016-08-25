module Thief.Term.TermCharSpec where

import Thief.Term.TermChar
import Thief.Term.Printable (width, height)
import Thief.Color (Color(..))
import SpecHelper

spec :: Spec
spec = describe "TermChar" $
  context "Printable" $ do
    it "calculates width & height" $ do
      let color = RGB 255 255 255
          sc = TC 'X' color color
          wc = TC 'ㅡ' color color
      width sc `shouldBe` 1
      width wc `shouldBe` 2
      height sc `shouldBe` 1
      height wc `shouldBe` 1
