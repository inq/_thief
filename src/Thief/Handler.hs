module Thief.Handler
  ( handlerLoop
  ) where

import Control.Monad (when)
import Misc.Default (def)
import Thief.Term.Ansi
  ( smcup, rmcup
  , movexy, moveCursor
  , queryCursorPos)
import Thief.Term.Classes (Printable(..))
import Thief.Term.Cursor (Cursor(..), move)
import Thief.Term.Brush (invert)
import Thief.Term.Buffer (borderedBuffer)
import Thief.Handler.Status (Status(..))
import qualified Misc.Color as Color
import qualified Control.Concurrent.Chan as C
import qualified Thief.Status as Stat
import qualified Thief.Raw as Raw



handlerLoop :: C.Chan Raw.Result -> IO ()
handlerLoop c = loop c def
  where
    loop c status = do
      ipt <- C.readChan c
      case status of
        Bare scr -> case ipt of
          Raw.Action (Raw.ResizeScreen s) -> do
            putStr queryCursorPos
            loop c $ Bare s
          Raw.Pair y x -> do
            putStr smcup
            case scr of
              Just (w, h) -> do
                putStr $ snd $ toAnsi def $
                  borderedBuffer borderBrush fillBrush w h
                loop c $ Ready (x, y) def
                  { theX = x, theY = y
                  , theWidth = w, theHeight = h
                  }
              Nothing -> putStrLn "Internal Error"
          _ -> loop c status
        Ready i cur -> case ipt of
          Raw.Action (Raw.ResizeScreen (Just (w, h))) -> do
            putStr $ snd $ toAnsi def $
              borderedBuffer borderBrush fillBrush w h
            loop c status { getCursor = move cur ipt }
          Raw.Action (Raw.ResizeScreen Nothing) ->
            putStrLn "== cannot inspect the terminal =="
          Raw.Char 'q' -> do
            putStr rmcup
            putStr $ uncurry movexy i
          _ -> do
            when (ipt == Raw.Char 'b') $ putStr "BOX"
            putStr $ moveCursor $ move cur ipt
            when (ipt /= Raw.None) $ putStr $ Stat.toStr ipt
            loop c status { getCursor = move cur ipt }
    borderBrush = invert def
    fillBrush = def
