module Main where

import Data.Yaml (decodeFileEither, ParseException)
import Data.Either (isLeft)
import System.Exit (exitFailure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Extensions.Shgif.Events (TickEvent(..), mainWithTick)
import Brick.Extensions.Shgif.Widgets (shgif)
import Shgif.Type (Shgif(..), updateShgif, addInitialCanvas)

type AppState = Shgif
data Name = NoName deriving (Eq, Ord)

-- You should use 'TickEvent' as Event type
app :: App AppState TickEvent Name
app = App {appDraw = ui
          , appHandleEvent = eHandler
          , appStartEvent = return
          , appChooseCursor = neverShowCursor
          , appAttrMap = const $ attrMap Vty.defAttr [] }

ui :: AppState -> [Widget Name]
ui s = [shgif s]

eHandler :: AppState -> BrickEvent Name TickEvent -> EventM Name (Next AppState)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (AppEvent Tick)  = liftIO (updateShgif s) >>= continue
eHandler s _ = continue s

main :: IO ()
main = do
    -- Read Shgif data from File
    sgf <- (decodeFileEither "docs/shgif-v2-format-example-page.yaml" :: IO (Either ParseException Shgif))

    when (isLeft sgf) $ exitFailure
    let fromRight (Right a) = a
    sgf' <- addInitialCanvas $ fromRight sgf

    finalState <- mainWithTick Nothing 400000 app sgf'
    return ()