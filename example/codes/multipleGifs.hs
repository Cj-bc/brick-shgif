module Main where

import Data.Yaml (decodeFileEither, ParseException)
import Data.Either (isLeft)
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Widgets.Core ((<+>))
import Brick.Widgets.Border (border)
import Brick.Extensions.Shgif.Events (TickEvent(..), mainWithTick)
import Brick.Extensions.Shgif.Widgets (shgif)
import Shgif.Type (Shgif(..))
import Shgif.Updater (updateNormal)
import Shgif.Loader (fromFiles)

type AppState = [Shgif]
data Name = NoName deriving (Eq, Ord)

-- You should use 'TickEvent' as Event type
app :: App AppState TickEvent Name
app = App {appDraw = ui
          , appHandleEvent = eHandler
          , appStartEvent = return
          , appChooseCursor = neverShowCursor
          , appAttrMap = const $ attrMap Vty.defAttr [] }

ui :: AppState -> [Widget Name]
ui s = [foldl1 (<+>) $ map (border . shgif) s]

eHandler :: AppState -> BrickEvent Name TickEvent -> EventM Name (Next AppState)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (AppEvent Tick)  = liftIO (sequence (map updateNormal s)) >>= continue
eHandler s _ = continue s

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ putStrLn "usage: multipleGifs <shgif-filename> [<shgif-filename>...]" >> exitSuccess

    -- Read Shgif data from File
    sgf <- fromFiles args :: IO (Either [ParseException] [Shgif])

    let fromLeft (Left c) = c
    when (isLeft sgf) $ putStrLn (unlines $ map show (fromLeft sgf)) >> exitFailure
    let (Right sgf') = sgf

    let ms = 1000
    finalState <- mainWithTick Nothing ms app sgf'
    return ()
