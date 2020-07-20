{-|
Module      : Shgif.Updater
Description : Collection of function which update Shgif status
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

This module provides lots kind of __updater__.

'Updater' is a function that'll update Shgif's internal tick-rate.

Each Updater has different update method.
-}
{-# LANGUAGE Rank2Types #-}
module Shgif.Updater (
    -- * Normal
      updateNormal

    -- With loop/reversed setting
    , updateNoLoop
    , updateReversed
    , updateReversedNoLoop

    -- * Flexible changing
    , updateTo
    , setTickTo
) where
import Shgif.Type.Internal
import Control.Lens (over, set, (+~), (&), (^.), (.~), view, _2)


update updateTick shgif = do
  newC <- shgifToCanvas $ updateTick shgif
  return $ set canvas (Just newC) $ updateTick shgif


-- | Update internal tick state, which will affect frame rendering.
--
-- This function __won't loop__ gif.
--
-- Use this if you want to show animation only once.
updateNoLoop :: Updater
updateNoLoop updatable = update updateTick updatable
    where
        lastTimeStamp = getLastTimeStamp updatable
        updateTick i | i <= lastTimeStamp = i + 1
                     | otherwise          = i


-- | Update internal tick state, which will affect frame rendering.  
--
-- This function reverse and __won't loop__ gif.
--
-- Use this if you want to show reversed animation for only once.
updateReversedNoLoop :: Updater
updateReversedNoLoop updatable = update updateTick updatable
    where
        updateTick i | 0 < i     = i - 1
                     | otherwise = i


-- | Update internal tick state, which will affect frame rendering.  
--
-- This function reverse gif.
--
-- Use this if you want to show reversed animation.
updateReversed :: Updater
updateReversed updatable = update updateTick updatable
    where
        lastTimeStamp = getLastTimeStamp updatable
        -- https://docs.unity3d.com/ja/2019.2/ScriptReference/Mathf.Repeat.html
        repeatReversed max val | val < 0   = max
                               | otherwise = val
        updateTick = repeatReversed lastTimeStamp . (subtract 1)

-- | Update internal tick state, which will affect frame rendering.  
updateNormal :: Updater
updateNormal updatable = update updateTick updatable
    where
        lastTimeStamp = getLastTimeStamp updatable
        -- https://docs.unity3d.com/ja/2019.2/ScriptReference/Mathf.Repeat.html
        repeat max val | max < val = 0
                       | otherwise = val
        updateTick :: Int -> Int
        updateTick = repeat lastTimeStamp . (+ 1)


-- | Update internal tick state to make it closer to given tick
updateTo :: Int -> Updater
updateTo tick updatable  = update updateTick updatable
    where
        updateTick i = case i `compare` tick of
                            LT -> i + 1
                            EQ -> i
                            GT -> i - 1


-- | Set internal tick state to given tick.
setTickTo :: Int -> Updater
setTickTo tick = update (const tick)


-- | Specialized Updater for 'Container'
--
-- Update each 'Shgif's with different Updaters
--
-- If you want to update Container synchronicity, use other 'Updater's
-- directly. They will update all Shgifs with same method.
updateContainer :: [Shgif -> IO Shgif] -> Container -> IO Container
updateContainer updaters container = shgifs (sequence . zipWith (\f v -> _2 f v) updaters) container
