module Main where

import Data.Yaml (decodeFileEither, ParseException)
import System.Exit (exitFailure)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Extensions.Shgif.Events (TickEvent(..), mainWithTick)
import Shgif.Type (Shgif(..), updateShgif, addInitialCanvas)

type AppState = Shgif

-- You should use 'TickEvent' as Event type
app :: App AppState TickEvent n
app = App {appDraw = ui
          , appHandleEvent = eHandler
          , appStartEvent = return
          , appAttrMap = const $ attrMap Vty.defAttr [] }

ui :: AppState -> [Widget n]
ui = Shgif

eHandler :: AppState -> BrickEvent n TickEvent -> EventM n (Next AppState)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (AppEvent Tick)  = updateShgif s >>= continue

main :: IO ()
main = do
    -- Read Shgif data from File
    sgf <- (decodeFileEither "docs/shgif-v2-format-example-page.yaml" :: IO (Either ParseException Shgif))

    when (isLeft sgf) $ exitFailure
    let fromRight (Right a) = a
    sgf' <- addInitialCanvas $ fromRight sgf

    finalState <- mainWithTick Nothing 1000 app sgf'
    return ()
