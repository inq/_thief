module Thief.Term.LineSpec where

import SpecHelper

import Thief.Term.Printable (width, height)
import Thief.Term.TChar (TChar(..))
import Thief.Color (Color(..), Brush(..))
import Thief.Term.Line


spec :: Spec
spec = describe "Line" $
  context "Printable" $
    it "calculates width & height" $ do
      let color = RGB 255 255 255
          sc = MkChar 'X' $ MkBrush color color
          wc = MkChar 'ㅡ' $ MkBrush color color
      width (MkLine [sc, wc, sc, wc]) `shouldBe` 6
