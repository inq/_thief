module Thief.Box where

import qualified Thief.Color  as Color
import qualified Thief.Ansi   as Ansi
import qualified Thief.Status as Stat

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
      topLine = Ansi.moveCursor topleft ++
          Ansi.setBackground b ++ take w (repeat ' ')
      content = concat $ map content' [y + 1..y + h - 2]
      content' y' = (Ansi.moveCursor $ left y') ++
          Ansi.setBackground b ++ " " ++
          Ansi.setBackground f ++ take (w - 2) (repeat ' ') ++
          Ansi.setBackground b ++ " "
      bottomLine = Ansi.moveCursor bottomleft ++
          Ansi.setBackground b ++ take w (repeat ' ')
      topleft = Stat.CursorPos x y 0 0
      bottomleft = Stat.CursorPos x (y + h - 1) 0 0
      left y' = Stat.CursorPos x y' 0 0
