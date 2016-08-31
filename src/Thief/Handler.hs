module Thief.Handler
  ( handlerLoop
  ) where

import Control.Monad (when)
import Misc.Default (def)
import Thief.Term.Ansi
  ( smcup, rmcup
  , moveCursor
  , queryCursorPos)
import Thief.Term.Classes (Printable(..))
import Thief.Term.Cursor (move)
import Thief.Term.Brush (invert)
import Thief.Term.Buffer (borderedBuffer)
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
            putStr $ snd $ toAnsi def $
              borderedBuffer borderBrush fillBrush w h
            putStr queryCursorPos
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
    borderBrush = invert def
    fillBrush = def
