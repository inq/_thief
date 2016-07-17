module Thief.IO
  ( initialize
  , runLoop
  ) where

import qualified Control.Concurrent.Chan as C
import qualified Thief.IO.Result as Res
import qualified Thief.IO.Output as Opt
import qualified Thief.IO.Input  as Ipt
import qualified Thief.IO.FFI    as FFI
import qualified Thief.IO.Ansi   as Ansi
import qualified Thief.Status    as Stat
import qualified Thief.Color     as Color

initialize :: IO ()
initialize = do
    Opt.initialize
    Ipt.initialize

runLoop :: C.Chan Res.Result -> IO ()
runLoop c = do
    res <- FFI.getTermSize
    case res of
      Just (w, h) -> do
          putStr $ Ansi.fillScreen w h Color.darkBlood
          Ipt.inputLoop $ Stat.defaultStatus w h
          Opt.finalize
      _ -> do
          Opt.finalize
          putStrLn "== cannot inspect the terminal =="
