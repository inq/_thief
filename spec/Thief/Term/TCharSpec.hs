module Thief.Term.TCharSpec where

import SpecHelper

import Thief.Term.Printable (width, height)
import Thief.Color (Color(..), Brush(..))
import Thief.Term.TChar


spec :: Spec
spec = describe "TChar" $
  context "Printable" $
    it "calculates width & height" $ do
      let color = RGB 255 255 255
          sc = MkChar (MkBrush color color) 'X'
          wc = MkChar (MkBrush color color) 'ㅡ'
      width sc `shouldBe` 1
      width wc `shouldBe` 2
      height sc `shouldBe` 1
      height wc `shouldBe` 1
