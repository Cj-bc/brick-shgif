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
import Control.Lens (over, set, (+~), (&), (^.), (.~), view)

-- | Update 'Shgif''s internal tick state, which will affect frame rendering.
--
-- This function __won't loop__ gif.
--
-- Use this if you want to show animation only once.
updateNoLoop :: Updater
updateNoLoop updatable = update updateTick updatable
    where
        lastTimeStamp = getLastTimeStamp updatable
        updateTick | (updatable^.getTick) <= lastTimeStamp = over getTick (+ 1)
                   | otherwise                             = id


-- | Update 'Shgif''s internal tick state, which will affect frame rendering.  
--
-- This function reverse and __won't loop__ gif.
--
-- Use this if you want to show reversed animation for only once.
updateReversedNoLoop :: Updater
updateReversedNoLoop updatable = update updateTick updatable
    where
        updateTick | 0 < (updatable^.getTick) = over getTick (subtract 1)
                   | otherwise                = id


-- | Update 'Shgif''s internal tick state, which will affect frame rendering.  
--
-- This function reverse gif.
--
-- Use this if you want to show reversed animation.
updateReversed :: Updater
updateReversed updatable = update updateTick updatable
    where
        lastTimeStamp = getLastTimeStamp updatable
        -- https://docs.unity3d.com/ja/2019.2/ScriptReference/Mathf.Repeat.html
        repeat max val | val < 0   = max
        updateTick = set getTick (repeat lastTimeStamp $ (updatable^.getTick) - 1)

-- | Update 'Shgif''s internal tick state, which will affect frame rendering.  
updateNormal :: Updater
updateNormal updatable = update updateTick updatable
    where
        lastTimeStamp = getLastTimeStamp updatable
        -- https://docs.unity3d.com/ja/2019.2/ScriptReference/Mathf.Repeat.html
        repeat max val | max < val = 0
                       | otherwise = val
        updateTick = set getTick (repeat lastTimeStamp $ (updatable^.getTick) + 1)


-- | Update 'Shgif''s internal tick state to make it closer to given tick
updateTo :: Int -> Updater
updateTo tick updatable  = update (getTick+~tickToAdd) updatable
    where
        tickToAdd = case (updatable^.getTick) `compare` tick of
                        LT -> 1
                        EQ -> 0
                        GT -> -1


-- | Set 'Shgif''s internal tick state to given tick.
setTickTo :: Int -> Updater
setTickTo tick = update (getTick.~tick)
