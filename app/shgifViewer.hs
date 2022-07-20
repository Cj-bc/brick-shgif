{-# LANGUAGE TemplateHaskell
           , OverloadedStrings
 #-}
module Main where

import Data.Yaml (decodeFileEither, ParseException)
import Data.Either (isLeft)
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import System.FSNotify (withManager, watchDir, Event(Modified))
import qualified System.FSNotify as FSN
import System.FilePath.Posix (takeDirectory)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, void, forever)
import Control.Lens (makeLenses, (^.), (&), (.~), over, set, use)
import qualified Graphics.Vty as Vty
import Brick
import Brick.BChan (newBChan, writeBChan, BChan)
import Brick.Extensions.Shgif.Widgets (shgif)
import Shgif.Type (Shgif(..))
import Shgif.Loader (fromFile)
import Shgif.Updater ( updateNormal
                     , updateReversed
                     , updateNoLoop
                     , updateReversedNoLoop
                     , updateNoLoop
                     )
import Options.Applicative

data AppState = AppState { _shellGif :: Shgif
                         , _updater :: (Shgif -> IO Shgif)
                         , _fileName :: FilePath
                         , _reloadedInfo :: Int
                         }
makeLenses ''AppState
data Name = NoName deriving (Eq, Ord)
data CustomEvent = Tick
                 | FileUpdated

-- Options {{{
data AppFlags = AppFlags { oneShot :: Bool
                         , reversed :: Bool
                         , speed :: Int
                         , watchFile :: Bool
                         }

revSwitch = flag True False

appFlags :: Parser (AppFlags, String)
appFlags = (,) <$> apf <*> filename
    where
        apf = AppFlags <$> switch (long "oneShot" <> help "play shgif only once (no loop)")
                       <*> switch (long "reversed" <> short 'r' <> help "play shgif backward")
                       <*> option auto (long "speed" <> value 1000 <> help "Speed to play in [micro second] (default: 1000)")
                       <*> switch (long "watch" <> short 'w' <> help "reload shgif when it's updated")
        filename = argument Options.Applicative.str (metavar "FILE")

optionInfo :: ParserInfo (AppFlags, String)
optionInfo = info (appFlags <**> helper) (fullDesc)
-- }}}

-- You should use 'CustomEvent' as Event type
app :: App AppState CustomEvent Name
app = App {appDraw = ui
          , appHandleEvent = eHandler
          , appStartEvent = return ()
          , appChooseCursor = neverShowCursor
          , appAttrMap = const $ attrMap Vty.defAttr [] }


-- UI {{{1
ui :: AppState -> [Widget Name]
ui s = [vBox [shgif (s^.shellGif)
             , updateInfo
             ]
       ]
    where
        updateInfo | (s^.reloadedInfo) <= 0 = emptyWidget
                   | otherwise              = Brick.str "shgif updated"


-- Event handler {{{1
eHandler :: BrickEvent Name CustomEvent -> EventM Name AppState ()
eHandler (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt
eHandler (AppEvent FileUpdated) = do
    shgif <- use shellGif
    fn <- use fileName
    (newsgf, ri) <- liftIO $ either (const (shgif, 0))
                              (\s -> (s, 1000)) <$> fromFile fn
    modify $ set shellGif newsgf . set reloadedInfo ri
eHandler (AppEvent Tick)  = do
    updater <- use updater
    shgif <- use shellGif
    newsgf <- liftIO $ updater shgif
    modify $ set shellGif newsgf . over reloadedInfo updateReloadedInfo
    where
        updateReloadedInfo 0 = 0
        updateReloadedInfo i = i - 1
eHandler _ = pure ()


-- helper functions {{{1
-- | Watch specified file and send FileUpdated event
-- when it's updated.
watchUpdate :: BChan CustomEvent -> FilePath -> IO ()
watchUpdate ch fn = void . forkIO . withManager $ \mng -> do
    watchDir
        mng
        (takeDirectory fn)
        isWatchedFile
        sendEvent
    forever $ threadDelay 10000
    where
        isWatchedFile :: FSN.Event -> Bool
        isWatchedFile e = case e of
                 Modified fn _ _ -> True
                 otherwise       -> False

        sendEvent :: FSN.Event -> IO ()
        sendEvent = \_ -> writeBChan ch FileUpdated


-- customMain wrapper to add Tick event
createBrickMain ch speed s = do
    void $ forkIO $ forever $ do
        writeBChan ch Tick
        threadDelay speed

    let mkvty = Vty.mkVty Vty.defaultConfig
    vty <- mkvty
    void $ customMain vty mkvty (Just ch) app s
-- }}}

main :: IO ()
main = do
    -- Option parsing {{{1
    (flag, filename) <- execParser optionInfo

    let f = case flag of
                (AppFlags True False  _ _) -> updateNoLoop
                (AppFlags True True   _ _) -> updateReversedNoLoop
                (AppFlags False False _ _) -> updateNormal
                (AppFlags False True  _ _) -> updateReversed
        AppFlags _ _ speed watch = flag
    -- }}}

    -- Read Shgif data from File {{{1
    sgf <- fromFile $ filename

    let fromLeft  (Left e)  = e
        fromRight (Right a) = a
    when (isLeft sgf) $ putStrLn ( show $ fromLeft sgf) >> exitFailure
    let (Right sgf') = sgf
    -- }}}

    let initialState = AppState sgf' f filename 0
    ch <- newBChan 10

    when watch $ watchUpdate ch filename
    void $ createBrickMain ch speed initialState
