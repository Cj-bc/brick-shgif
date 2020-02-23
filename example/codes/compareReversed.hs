module Main where

import Data.Yaml (decodeFileEither, ParseException)
import Data.Either (isLeft)
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Widgets.Border (border)
import Brick.Extensions.Shgif.Events (TickEvent(..), mainWithTick)
import Brick.Extensions.Shgif.Widgets (shgif)
import Shgif.Type (Shgif(..))
import Shgif.Loader (fromFile)
import Shgif.Updater (updateShgif, updateShgifReversed)

type AppState = (Shgif, Shgif)
data Name = NoName deriving (Eq, Ord)

-- You should use 'TickEvent' as Event type
app :: App AppState TickEvent Name
app = App {appDraw = ui
          , appHandleEvent = eHandler
          , appStartEvent = return
          , appChooseCursor = neverShowCursor
          , appAttrMap = const $ attrMap Vty.defAttr [] }

ui :: AppState -> [Widget Name]
ui (s, r) = [vBox [border (shgif s), str "original"] <+>
             vBox [border (shgif r), str "reversed"]]

eHandler :: AppState -> BrickEvent Name TickEvent -> EventM Name (Next AppState)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler (s, r) (AppEvent Tick)  = continue =<< liftIO (do
                            ns <- updateShgif s
                            nr <- updateShgifReversed r
                            return (ns, nr))
eHandler s _ = continue s

main :: IO ()
main = do
    args <- getArgs
    when (args == []) $ putStrLn "usage: compareReversed <shgif-filename>" >> exitSuccess

    -- Read Shgif data from File
    sgf <- fromFile $ head args
    sgfRev <- fromFile $ head args

    -- Check if reading data is done correctly
    let fromLeft  (Left e)  = e
        fromRight (Right a) = a
    when (isLeft sgf) $ putStrLn ( show $ fromLeft sgf) >> exitFailure
    let (Right sgf')    = sgf
        (Right sgfRev') = sgfRev

    let ms = 1000
    finalState <- mainWithTick Nothing ms app (sgf', sgfRev')
    return ()
