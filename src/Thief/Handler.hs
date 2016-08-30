module Thief.Handler
  ( handlerLoop
  ) where

import qualified Control.Concurrent.Chan as C
import qualified Control.Monad        as M
import qualified Thief.Handler.Cursor as Cur
import qualified Thief.Handler.Ansi   as Ansi
import qualified Thief.Status         as Stat
import qualified Thief.Raw            as Raw
import qualified Thief.Color          as Color
import qualified Thief.Box            as Box

handlerLoop :: C.Chan Raw.Result -> IO ()
handlerLoop c = loop c Cur.defaultCursor
  where
    loop c cur = do
      ipt <- C.readChan c
      case ipt of
        Raw.Action (Raw.ResizeScreen (Just (w, h))) -> do
            putStr Ansi.smcup
            putStr $ Ansi.fillScreen w h Color.darkBlood
            putStrLn Ansi.queryCursorPos
            loop c $ Cur.move cur ipt
        Raw.Action (Raw.ResizeScreen Nothing) ->
            putStrLn "== cannot inspect the terminal =="
        Raw.Char 'q' ->
            putStr Ansi.rmcup
        _ -> do
            M.when (ipt == Raw.Char 'b') $ do
                putStr $ show theBox
            putStr $ Ansi.moveCursor $ Cur.move cur ipt
            M.when (ipt /= Raw.None) $ putStr $ Stat.toStr ipt
            loop c $ Cur.move cur ipt
    borderColor = Color.lightGray
    fillColor = Color.darkBlue
    theBox = Box.Box 30 30 80 30 borderColor fillColor
