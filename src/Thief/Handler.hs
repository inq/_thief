module Thief.Handler
  ( handlerLoop
  ) where

import Control.Monad (when)
import Misc.Default (def)
import Thief.Term.Ansi
  ( smcup, rmcup
  , fillScreen, moveCursor
  , queryCursorPos)
import Thief.Term.Cursor (move)
import qualified Misc.Color as Color
import qualified Control.Concurrent.Chan as C
import qualified Thief.Status as Stat
import qualified Thief.Raw as Raw



handlerLoop :: C.Chan Raw.Result -> IO ()
handlerLoop c = loop c def
  where
    loop c cur = do
      ipt <- C.readChan c
      case ipt of
        Raw.Action (Raw.ResizeScreen (Just (w, h))) -> do
            putStr smcup
            putStr $ fillScreen w h Color.darkBlood
            putStrLn queryCursorPos
            loop c $ move cur ipt
        Raw.Action (Raw.ResizeScreen Nothing) ->
            putStrLn "== cannot inspect the terminal =="
        Raw.Char 'q' ->
            putStr rmcup
        _ -> do
            when (ipt == Raw.Char 'b') $ putStr "BOX"
            putStr $ moveCursor $ move cur ipt
            when (ipt /= Raw.None) $ putStr $ Stat.toStr ipt
            loop c $ move cur ipt
    borderColor = Color.lightGray
    fillColor = Color.darkBlue
