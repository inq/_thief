module Thief.Term.BufferSpec where

import SpecHelper

import Thief.Term.Printable (width, height)
import Thief.Term.Line (Line(..))
import Thief.Term.TChar (TChar(..))
import Thief.Color (Color(..), Brush(..))
import Thief.Term.Buffer


spec :: Spec
spec = describe "Buffer" $
  context "Printable" $
    it "calculates width & height" $ do
      let color = RGB 255 255 255
          sc = MkChar 'X' $ MkBrush color color
          wc = MkChar 'ㅡ' $ MkBrush color color
      width (MkBuffer [MkLine [sc, wc, sc, wc]]) `shouldBe` 6
