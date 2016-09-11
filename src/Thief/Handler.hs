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
  , civis, cvvis, smcup, rmcup, movexy, moveCur, queryCursorPos
  )
import Thief.Handler.Status (Status(..))
import Thief.UI.Screen (initScreen, rotateFocus)
import Thief.UI.Common (Drawable(..), Size(..), Resizable(..))
import Thief.Raw (Event(..))

-- * Type Alises

type Handler = StateT Status (Writer String) ()

exit :: Handler
-- ^ Exit the handler
exit = put Terminated

throwError :: String -> Handler
-- ^ Exit with error
throwError msg = tell msg >> put Terminated

handler :: Status -> Event -> Handler
-- ^ The core handler
handler (Bare scr) e = case e of
    ResizeScreen s -> do
        tell queryCursorPos
        put $ Bare s
    Pair y x -> do
        tell smcup
        case scr of
            Just (w, h) -> do
                let scr = initScreen def $ MkSize w h
                tell $ movexy 0 0
                tell $ snd $ toAnsi def $ draw scr
                put $ Ready (x, y) scr def
                    { theX = x, theY = y
                    , theWidth = w, theHeight = h
                    }
            Nothing -> throwError "Internal Error"
    _ -> return ()
handler (Ready orig scr cur) e = case e of
    ipt@(ResizeScreen (Just (w, h))) -> do
        let scr' = resize scr $ MkSize w h
        tell $ civis ++ movexy 0 0 ++ (snd $ toAnsi def $ draw scr') ++ cvvis ++ moveCur cur
        modify (\x -> x
                 { getScreen = scr'
                 , getCursor = cur { theWidth = w, theHeight = h }
                 })
    ResizeScreen Nothing ->
        throwError "Cannot inpect the terminal"
    Char 'q' ->
        exit
    Char '\ETB' -> do
        let scr' = rotateFocus scr
        tell $ civis ++ movexy 0 0 ++ (snd $ toAnsi def $ draw scr') ++ cvvis ++ moveCur cur
        modify (\x -> x{ getScreen = scr' })
    ipt -> do
        when (ipt == Char 'b') $ tell "BOX"
        tell $ moveCur $ moveCursor cur ipt
        modify (\x -> x { getCursor = moveCursor cur ipt })

handlerLoop :: Chan Event -> IO ()
-- ^ The main loop
handlerLoop c = loop c def
  where
    loop c status = do
      ipt <- readChan c
      let ((_, res), str) = runWriter $ runStateT (handler status ipt) status
      case res of
          Terminated -> case status of
              Ready orig _ _ -> do
                 putStr $ rmcup ++ uncurry movexy orig ++ str
              _ -> return ()
          _ -> do
              putStr str
              loop c res
