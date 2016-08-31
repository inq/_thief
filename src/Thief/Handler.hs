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

handler :: Status -> Raw.Result -> IO Status
handler status ipt =
    case status of
      Bare scr -> case ipt of
        Raw.Action (Raw.ResizeScreen s) -> do
          putStr queryCursorPos
          return $ Bare s
        Raw.Pair y x -> do
          putStr smcup
          case scr of
            Just (w, h) -> do
              putStr $ snd $ toAnsi def $
                borderedBuffer (invert def) def w h
              return $ Ready (x, y) def
                { theX = x, theY = y
                , theWidth = w, theHeight = h
                }
            Nothing -> do
              putStrLn "Internal Error"
              return Terminated
        _ -> return status
      Ready i cur -> case ipt of
        Raw.Action (Raw.ResizeScreen (Just (w, h))) -> do
          putStr $ snd $ toAnsi def $
            borderedBuffer (invert def) def w h
          return status { getCursor = move cur ipt }
        Raw.Action (Raw.ResizeScreen Nothing) -> do
          putStrLn "== cannot inspect the terminal =="
          return Terminated
        Raw.Char 'q' -> do
          putStr rmcup
          putStr $ uncurry movexy i
          return Terminated
        _ -> do
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
