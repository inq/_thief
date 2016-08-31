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
import qualified Misc.Color as Color
import qualified Thief.Status as Stat
import qualified Thief.Raw as Raw

exit :: (Int, Int) -> String -> IO Status
exit orig msg = do
    putStr rmcup
    putStr $ uncurry movexy orig
    return Terminated

handler :: Status -> Raw.Result -> IO Status
handler status@(Bare scr) = handle
  where
    handle (Raw.Action (Raw.ResizeScreen s)) = do
        putStr queryCursorPos
        return $ Bare s
    handle (Raw.Pair y x) = do
        putStr smcup
        case scr of
            Just (w, h) -> do
                putStr $ snd $ toAnsi def $
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
        putStr $ snd $ toAnsi def $
            borderedBuffer (invert def) def w h
        return status { getCursor = move cur ipt }
    handle (Raw.Action (Raw.ResizeScreen Nothing)) =
        exit orig "Cannot inpect the terminal"
    handle (Raw.Char 'q') =
        exit orig "Have a nice day!"
    handle ipt = do
        when (ipt == Raw.Char 'b') $ putStr "BOX"
        putStr $ moveCursor $ move cur ipt
        when (ipt /= Raw.None) $ putStr $ Stat.toStr ipt
        return status { getCursor = move cur ipt }

handlerLoop :: Chan Raw.Result -> IO ()
handlerLoop c = loop c def
  where
    loop c status = do
      ipt <- readChan c
      res <- handler status ipt
      case res of
          Terminated -> return ()
          _ -> loop c res
