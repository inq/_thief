module Thief.Handler
  ( handlerLoop
  ) where

import Control.Monad (when)
import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Monad.State (StateT, runStateT, get, put, modify)
import Misc (def)
import Thief.Term
  ( Printable(toAnsi)
  , Cursor(theX, theY, theWidth, theHeight), moveCursor
  , invertBrush
  , borderedBuffer
  , smcup, rmcup, movexy, moveCur, queryCursorPos
  )
import Thief.Handler.Status (Status(..))
import Thief.UI.Screen (initScreen)
import Thief.UI.Common (Drawable(..), Size(..), Resizable(..))
import qualified Thief.Raw as Raw

-- * Type Alises

type Handler = StateT Status (Writer String) ()

exit :: String -> Handler
-- ^ Exit the handler
exit msg = tell msg >> put Terminated

handler :: Status -> Raw.Result -> Handler
-- ^ The core handler
handler (Bare scr) = handle
  where
    handle (Raw.Action (Raw.ResizeScreen s)) = do
        tell queryCursorPos
        put $ Bare s
    handle (Raw.Pair y x) = do
        tell smcup
        case scr of
            Just (w, h) -> do
                let scr = initScreen $ MkSize w h
                tell $ movexy 0 0
                tell $ snd $ toAnsi def $ draw scr
                put $ Ready (x, y) scr def
                    { theX = x, theY = y
                    , theWidth = w, theHeight = h
                    }
            Nothing -> exit "Internal Error"
    handle _ = return ()
handler (Ready orig scr cur) = handle
  where
    handle ipt@(Raw.Action (Raw.ResizeScreen (Just (w, h)))) = do
        let scr' = resize scr $ MkSize w h
        tell $ movexy 0 0
        tell $ snd $ toAnsi def $ draw scr'
        modify (\x -> x
                 { getScreen = scr'
                 , getCursor = cur { theWidth = w, theHeight = h }
                 })
    handle (Raw.Action (Raw.ResizeScreen Nothing)) =
        exit "Cannot inpect the terminal"
    handle (Raw.Char 'q') =
        exit "Have a nice day!"
    handle ipt = do
        when (ipt == Raw.Char 'b') $ tell "BOX"
        tell $ moveCur $ moveCursor cur ipt
        when (ipt /= Raw.None) $ tell $ show ipt
        modify (\x -> x { getCursor = moveCursor cur ipt })

handlerLoop :: Chan Raw.Result -> IO ()
-- ^ The main loop
handlerLoop c = loop c def
  where
    loop c status = do
      ipt <- readChan c
      let ((_, res), str) = runWriter $ runStateT (handler status ipt) status
      putStr str
      case res of
          Terminated -> case status of
              Ready orig _ _ -> do
                 putStr rmcup
                 putStr $ uncurry movexy orig
              _ -> return ()
          _ -> loop c res
