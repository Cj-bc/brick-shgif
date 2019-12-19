module Brick.Extensions.Shgif.Events where

import Data.Maybe (fromMaybe)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Brick
import Brick.BChan
import Graphics.Vty

-- | Simple Tick only Event
-- Use this if you don't have any other custom Events
data TickEvent = Tick

-- | Create @customMain@ with Tick event support
--  difference between this & customMain:
--    - Create new thread to Generate TickEvent
--    - Generate 'TickEvent' every 'tickRate' microseconds.
--
-- you should count up something in event handler
-- if you want to know how much time does TickEvent ran.
--
-- Those codes are from tutorials:
-- - jtdaugherty/brick/docs/samtay-tutorial.md @46feab9bfef540e62bbe05424297959ece2a711a
-- - jtdaugherty/brick/docs/guide.rst#using-your-own-event-type @46feab9bfef540e62bbe05424297959ece2a711a
mainWithTick :: Ord n => Maybe Int -> Int -> App state TickEvent n -> state -> IO state
mainWithTick chanCapacity tickRate app initialState = do
    eventChan <- Brick.BChan.newBChan $ fromMaybe 10 chanCapacity

    forkIO $ forever $ do
      writeBChan eventChan Tick
      threadDelay tickRate

    let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
    customMain buildVty (Just eventChan) app initialState
