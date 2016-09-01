module Thief.Term.LineSpec where

import SpecHelper

import Misc (def)
import Thief.Term.Brush (invertBrush)
import Thief.Term.Classes (Printable(..))
import Thief.Term.Line


spec :: Spec
spec = describe "Line" $
  context "Printable" $ do
    it "calculates width & height" $ do
      width (fromString def "ｆｕｌlｗiｄth") `shouldBe` 14
      width (leftAligned def 3 "ｆｕｌlｗiｄth") `shouldBe` 3
    it "generate ansi string" $ do
      let line = fromString def "ｆｕｌlｗiｄth"
      toAnsi (invertBrush def) line `shouldBe`
        ( def
        , "\ESC[38;2;200;200;200m\ESC[48;2;50;50;50mｆｕｌlｗiｄth"
        )
