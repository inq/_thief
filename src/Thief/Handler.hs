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
import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Monad.State (StateT, runStateT, get, put, modify)
import qualified Misc.Color as Color
import qualified Thief.Status as Stat
import qualified Thief.Raw as Raw

type Handler = StateT Status (Writer String) ()

exit :: String -> Handler
exit msg = tell msg >> put Terminated

handler :: Status -> Raw.Result -> Handler
handler (Bare scr) = handle
  where
    handle (Raw.Action (Raw.ResizeScreen s)) = do
        tell queryCursorPos
        put $ Bare s
    handle (Raw.Pair y x) = do
        tell smcup
        case scr of
            Just (w, h) -> do
                tell $ movexy 0 0
                tell $ snd $ toAnsi def $
                    borderedBuffer (invert def) def w h
                put $ Ready (x, y) def
                    { theX = x, theY = y
                    , theWidth = w, theHeight = h
                    }
            Nothing -> exit "Internal Error"
    handle _ = return ()
handler (Ready orig cur) = handle
  where
    handle ipt@(Raw.Action (Raw.ResizeScreen (Just (w, h)))) = do
        tell $ movexy 0 0
        tell $ snd $ toAnsi def $
            borderedBuffer (invert def) def w h
        modify (\x -> x { getCursor = cur { theWidth = w, theHeight = h } })
    handle (Raw.Action (Raw.ResizeScreen Nothing)) =
        exit "Cannot inpect the terminal"
    handle (Raw.Char 'q') =
        exit "Have a nice day!"
    handle ipt = do
        when (ipt == Raw.Char 'b') $ tell "BOX"
        tell $ moveCursor $ move cur ipt
        when (ipt /= Raw.None) $ tell $ Stat.toStr ipt
        modify (\x -> x { getCursor = move cur ipt })

handlerLoop :: Chan Raw.Result -> IO ()
handlerLoop c = loop c def
  where
    loop c status = do
      ipt <- readChan c
      let ((_, res), str) = runWriter $ runStateT (handler status ipt) status
      putStr str
      case res of
          Terminated -> case status of
              Ready orig _ -> do
                 putStr rmcup
                 putStr $ uncurry movexy orig
              _ -> return ()
          _ -> loop c res
