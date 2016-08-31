module Thief.Term.LineSpec where

import SpecHelper

import Misc.Default (def)
import Thief.Term.Printable (width, toAnsi)
import Thief.Color (invert)
import Thief.Term.Line


spec :: Spec
spec = describe "Line" $
  context "Printable" $ do
    it "calculates width & height" $ do
      width (fromString def "ｆｕｌlｗiｄth") `shouldBe` 14
      width (leftAligned def 3 "ｆｕｌlｗiｄth") `shouldBe` 3
    it "generate ansi string" $ do
      let line = fromString def "ｆｕｌlｗiｄth"
      toAnsi (invert def) line `shouldBe`
        ( def
        , "\ESC[38;2;200;200;200m\ESC[48;2;50;50;50mｆｕｌlｗiｄth"
        )
