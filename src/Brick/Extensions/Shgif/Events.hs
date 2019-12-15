module Brick.Extensions.Shgif.Events where

import Data.Maybe (fromMaybe)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Brick
import Brick.BChan
import Graphics.Vty

data TickEvent = Tick Int

-- Create @customMain@ with Tick event support
--  difference between this & customMain:
--    - Generate 'TickEvent' for specified 
--
-- Those codes are from tutorials:
-- - jtdaugherty/brick/docs/samtay-tutorial.md @46feab9bfef540e62bbe05424297959ece2a711a
-- - jtdaugherty/brick/docs/guide.rst#using-your-own-event-type @46feab9bfef540e62bbe05424297959ece2a711a
mainWithTick :: Maybe Int -> Int -> App s (Int -> TickEvent) n -> state -> IO state
mainWithTick chanCapacity tickRate app initialState = do
    eventChan <- Brick.BChan.newBChan $ fromMaybe 10 chanCapacity
    forkIO $ forever $ do
      writeBChan eventChan Tick
      threadDelay tickRate
    let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
    customMain buildVty (Just eventChan) app initialState
    -- Use finalState and exit
