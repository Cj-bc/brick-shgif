{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick
import Brick.Extensions.Shgif.Widgets
import Brick.Extensions.Shgif.Events
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Tart.Format (readTartFile, sortedCanvases, TartFile(..), toTartFilepath)
import Shgif.Type (Shgif)
import Shgif.Updater (updateShgif)
import Shgif.Loader (fromTartFile)
import qualified Graphics.Vty as Vty

type AppState = Shgif
data Name = NoName deriving (Eq, Ord)

ui = pure . shgif

eHandler :: AppState -> BrickEvent Name TickEvent -> EventM Name (Next AppState)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (AppEvent Tick) = liftIO (updateShgif s) >>= continue
eHandler s _ = continue s

app = App {appDraw = ui
          , appHandleEvent = eHandler
          , appStartEvent = return
          , appChooseCursor = neverShowCursor
          , appAttrMap = const $ attrMap Vty.defAttr []
          }

main = do
    tartfile <- readTartFile $ toTartFilepath "../resources/tart/number.tart"
    case tartfile of
        Left e  -> putStrLn e
        Right t -> void $ mainWithTick Nothing 1000 app (fromTartFile Nothing t)
