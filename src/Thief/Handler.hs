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
import qualified Misc.Color as Color
import qualified Thief.Status as Stat
import qualified Thief.Raw as Raw

exit :: (Int, Int) -> String -> Writer String Status
exit orig msg = do
    tell rmcup
    tell $ uncurry movexy orig
    return Terminated

handler :: Status -> Raw.Result -> Writer String Status
handler status@(Bare scr) = handle
  where
    handle (Raw.Action (Raw.ResizeScreen s)) = do
        tell queryCursorPos
        return $ Bare s
    handle (Raw.Pair y x) = do
        tell smcup
        case scr of
            Just (w, h) -> do
                tell $ movexy 0 0
                tell $ snd $ toAnsi def $
                    borderedBuffer (invert def) def w h
                return $ Ready (x, y) def
                    { theX = x, theY = y
                    , theWidth = w, theHeight = h
                    }
            Nothing -> exit (x, y) "Internal Error"
    handle _ = return status
handler status@(Ready orig cur) = handle
  where
    handle ipt@(Raw.Action (Raw.ResizeScreen (Just (w, h)))) = do
        tell $ movexy 0 0
        tell $ snd $ toAnsi def $
            borderedBuffer (invert def) def w h
        return status { getCursor = cur { theWidth = w, theHeight = h } }
    handle (Raw.Action (Raw.ResizeScreen Nothing)) =
        exit orig "Cannot inpect the terminal"
    handle (Raw.Char 'q') =
        exit orig "Have a nice day!"
    handle ipt = do
        when (ipt == Raw.Char 'b') $ tell "BOX"
        tell $ moveCursor $ move cur ipt
        when (ipt /= Raw.None) $ tell $ Stat.toStr ipt
        return status { getCursor = move cur ipt }

handlerLoop :: Chan Raw.Result -> IO ()
handlerLoop c = loop c def
  where
    loop c status = do
      ipt <- readChan c
      let (res, str) = runWriter $ handler status ipt
      putStr str
      case res of
          Terminated -> return ()
          _ -> loop c res
