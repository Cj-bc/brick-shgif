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
      updateShgif

    -- With loop/reversed setting
    , updateShgifNoLoop
    , updateShgifReversed
    , updateShgifReversedNoLoop

    -- * Flexible changing
    , updateShgifTo
    , setShgifTickTo
) where
import Shgif.Type.Internal
import Control.Lens (over, set, (+~), (&), (^.), (.~), view)

-- | Update 'Shgif''s internal tick state, which will affect frame rendering.
--
-- As 'updateShgif' has type `Shgif -> IO Shgif`, it can be called inside 'Brick.EventM' monad.
--
-- This function __won't loop__ gif.
--
-- Use this if you want to show animation only once.
updateShgifNoLoop :: Updater
updateShgifNoLoop shgif = update updateTick shgif
    where
        lastTimeStamp = getLastTimeStamp shgif
        updateTick | (shgif^.getTick) <= lastTimeStamp = over getTick (+ 1)
                   | otherwise                             = id


-- | Update 'Shgif''s internal tick state, which will affect frame rendering.  
--
-- As 'updateShgif' has type `Shgif -> IO Shgif`, it can be called inside brick's 'Brick.EventM' monad.
--
-- This function reverse and __won't loop__ gif.
--
-- Use this if you want to show reversed animation for only once.
updateShgifReversedNoLoop :: Updater
updateShgifReversedNoLoop shgif = update updateTick shgif
    where
        updateTick | 0 < (shgif^.getTick) = over getTick (subtract 1)
                   | otherwise                = id


-- | Update 'Shgif''s internal tick state, which will affect frame rendering.  
--
-- As 'updateShgif' has type `Shgif -> IO Shgif`, it can be called inside brick's 'Brick.EventM' monad.
--
-- This function reverse gif.
--
-- Use this if you want to show reversed animation.
updateShgifReversed :: Updater
updateShgifReversed shgif = update updateTick shgif
    where
        lastTimeStamp = getLastTimeStamp shgif
        -- https://docs.unity3d.com/ja/2019.2/ScriptReference/Mathf.Repeat.html
        repeat max val | val < 0   = max
        updateTick = set getTick (repeat lastTimeStamp $ (shgif^.getTick) - 1)

-- | Update 'Shgif''s internal tick state, which will affect frame rendering.  
--
-- As 'updateShgif' has type `Shgif -> IO Shgif`, it can be called inside brick's 'Brick.EventM' monad.
--
updateShgif :: Updater
updateShgif shgif = update updateTick shgif
    where
        lastTimeStamp = getLastTimeStamp shgif
        -- https://docs.unity3d.com/ja/2019.2/ScriptReference/Mathf.Repeat.html
        repeat max val | max < val = 0
                       | otherwise = val
        updateTick = set getTick (repeat lastTimeStamp $ (shgif^.getTick) + 1)


-- | Update 'Shgif''s internal tick state to make it closer to given tick
updateShgifTo :: Int -> Updater
updateShgifTo tick shgif  = update (getTick+~tickToAdd) shgif
    where
        tickToAdd = case (shgif^.getTick) `compare` tick of
                        LT -> 1
                        EQ -> 0
                        GT -> -1


-- | Set 'Shgif''s internal tick state to given tick.
setShgifTickTo :: Int -> Updater
setShgifTickTo tick = update (getTick.~tick)
