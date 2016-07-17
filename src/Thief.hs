module Thief where

import qualified Thief.Input        as Ipt
import qualified Thief.Output       as Opt
import qualified Thief.Status       as Stat
import qualified Thief.Ansi         as Ansi
import qualified Thief.Color        as Color
import qualified Thief.Internal.FFI as FFI

mainLoop :: IO ()
mainLoop = do
    Opt.initialize
    Ipt.initialize
    res <- FFI.getTermSize
    case res of
      Just (w, h) -> do
          putStr $ Ansi.fillScreen w h Color.darkBlood
          Ipt.inputLoop $ Stat.defaultStatus w h
          Opt.finalize
      _ -> do
          Opt.finalize
          putStrLn "== cannot inspect the terminal =="
