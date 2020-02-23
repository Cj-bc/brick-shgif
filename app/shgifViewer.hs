module Main where

import Data.Yaml (decodeFileEither, ParseException)
import Data.Either (isLeft)
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Extensions.Shgif.Events (TickEvent(..), mainWithTick)
import Brick.Extensions.Shgif.Widgets (shgif)
import Shgif.Type (Shgif(..))
import Shgif.Loader (fromFile)
import Shgif.Updater ( updateShgif
                     , updateShgifReversed
                     , updateShgifNoLoop
                     , updateShgifReversedNoLoop
                     , updateShgifNoLoop
                     )
import Options.Applicative

type AppState = (Shgif, (Shgif -> IO Shgif))
data Name = NoName deriving (Eq, Ord)

-- Options {{{
data AppFlags = AppFlags { oneShot :: Bool
                         , reversed :: Bool
                         }

revSwitch = flag True False

appFlags :: Parser (AppFlags, String)
appFlags = (,) <$> apf <*> filename
    where
        apf = AppFlags <$> switch (long "oneShot" <> help "play shgif only once (no loop)")
                       <*> switch (long "reversed" <> short 'r' <> help "play shgif backward")
        filename = argument Options.Applicative.str (metavar "FILE")

optionInfo :: ParserInfo (AppFlags, String)
optionInfo = info (appFlags <**> helper) (fullDesc)
-- }}}

-- You should use 'TickEvent' as Event type
app :: App AppState TickEvent Name
app = App {appDraw = ui
          , appHandleEvent = eHandler
          , appStartEvent = return
          , appChooseCursor = neverShowCursor
          , appAttrMap = const $ attrMap Vty.defAttr [] }

ui :: AppState -> [Widget Name]
ui (s, _) = [shgif s]

eHandler :: AppState -> BrickEvent Name TickEvent -> EventM Name (Next AppState)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler (sgf, f) (AppEvent Tick)  = liftIO (f sgf) >>= continue . flip (,) f
eHandler s _ = continue s

main :: IO ()
main = do
    (flag, filename) <- execParser optionInfo

    let f = case flag of
                (AppFlags True False)  -> updateShgifNoLoop
                (AppFlags True True)   -> updateShgifReversedNoLoop
                (AppFlags False False) -> updateShgif
                (AppFlags False True)  -> updateShgifReversed

    -- Read Shgif data from File
    sgf <- fromFile $ filename

    let fromLeft  (Left e)  = e
        fromRight (Right a) = a
    when (isLeft sgf) $ putStrLn ( show $ fromLeft sgf) >> exitFailure
    let (Right sgf') = sgf

    let ms = 1000
    finalState <- mainWithTick Nothing ms app (sgf', f)
    return ()
