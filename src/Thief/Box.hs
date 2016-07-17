module Thief.Box
  ( Box (..)
  ) where

import qualified Thief.Color   as Color
import qualified Thief.Raw.Ansi as Ansi

data Box = Box
  { posX :: Int
  , posY :: Int
  , width :: Int
  , height :: Int
  , borderColor :: Color.Color
  , fillColor :: Color.Color
  }

instance Show Box where
  show (Box x y w h b f) = topLine ++ content ++ bottomLine
    where
      topLine = Ansi.move x y ++ Ansi.spaces b w
      content = concat $ map content' [y + 1..y + h - 2]
        where
          content' y' = concat
            [ Ansi.move x y'
            , Ansi.spaces b 1
            , Ansi.spaces f (w - 2)
            , Ansi.spaces b 1
            ]
      bottomLine = Ansi.move x (y + h - 1) ++ Ansi.spaces b w
