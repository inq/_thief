module Thief.Term.LineSpec where

import SpecHelper

import Thief.Term.Printable (width, height)
import Thief.Term.TChar (TChar(..))
import Thief.Color (Color(..), Brush(..))
import Thief.Term.Line


spec :: Spec
spec = describe "Line" $
  context "Printable" $ do
    it "calculates width & height" $ do
      let color = RGB 255 255 255
          sc = MkChar (MkBrush color color) 'X'
          wc = MkChar (MkBrush color color) 'ㅡ'
      width (MkLine [sc, wc, sc, wc]) `shouldBe` 6
    it "produces left-aligned line" $ do
      let color = RGB 255 255 255
          br = MkBrush color color
      width (leftAligned br 3 "Hello") `shouldBe` 3
