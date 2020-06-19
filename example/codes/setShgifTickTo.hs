-- Please execute this from project root dir
-- (Problem of path solving)
module Main where

import Brick
import Brick.Extensions.Shgif.Widgets (shgif)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)
import qualified Graphics.Vty as Vty
import Shgif.Type (Shgif(..))
import Shgif.Loader (fromFile)
import Shgif.Updater (setTickTo)
import System.Exit (exitFailure)
import System.Environment (getArgs)

type AppState = Shgif

ui s = [shgif s]

eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar k) []))
    | k `elem` "0123456789" = do
        sgf <- liftIO $ setTickTo (read [k]) s
        continue sgf
    | otherwise             = continue s
eHandler s _ = continue s

app :: App AppState () ()
app = App { appDraw = ui
          , appHandleEvent = eHandler
          , appStartEvent  = return
          , appChooseCursor = neverShowCursor
          , appAttrMap = const $ attrMap Vty.defAttr []
          }

main = do
    sgf <- fromFile "example/shgifs/tenPanels.yaml"
    case sgf of
        Left e -> print e >> exitFailure
        Right sgf -> return ()
    let (Right sgf') = sgf

    void $ defaultMain app sgf'
