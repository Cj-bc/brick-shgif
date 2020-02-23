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
module Shgif.Updater (
    -- * Normal
      updateShgif

    -- With loop/reversed setting
    , updateShgifNoLoop
    , updateShgifReversed
    , updateShgifReversedNoLoop

    -- * Flexible changing
    , updateShgifTo
) where
import Shgif.Type.Internal
import Control.Lens (over, set, (+~), (&), (^.))


-- | Update 'Shgif''s internal tick state, which will affect frame rendering.
--
-- As 'updateShgif' has type `Shgif -> IO Shgif`, it can be called inside 'Brick.EventM' monad.
--
-- This function __won't loop__ gif.
--
-- Use this if you want to show animation only once.
updateShgifNoLoop :: Shgif -> IO Shgif
updateShgifNoLoop shgif@(Shgif t a f w h tick ds c) = do
    let updateTick | tick <= lastTimeStamp = over currentTick (+ 1)
                   | otherwise             = id

    newC <- shgifToCanvas $ updateTick shgif

    return $ set canvas (Just newC) $ updateTick shgif
    where
        lastTimeStamp = maximum $ map fst ds


-- | Update 'Shgif''s internal tick state, which will affect frame rendering.  
--
-- As 'updateShgif' has type `Shgif -> IO Shgif`, it can be called inside brick's 'Brick.EventM' monad.
--
-- This function reverse and __won't loop__ gif.
--
-- Use this if you want to show reversed animation for only once.
updateShgifReversedNoLoop :: Shgif -> IO Shgif
updateShgifReversedNoLoop shgif = do
    let updateTick | 0 < (shgif^.currentTick) = over currentTick (subtract 1)
                   | otherwise                = id
    newC <- shgifToCanvas $ updateTick shgif
    return $ set canvas (Just newC) $ updateTick shgif


-- | Update 'Shgif''s internal tick state, which will affect frame rendering.  
--
-- As 'updateShgif' has type `Shgif -> IO Shgif`, it can be called inside brick's 'Brick.EventM' monad.
--
-- This function reverse gif.
--
-- Use this if you want to show reversed animation.
updateShgifReversed :: Shgif -> IO Shgif
updateShgifReversed shgif = do
    let updateTick = set currentTick (repeat lastTimeStamp $ (shgif^.currentTick) - 1)
    newC <- shgifToCanvas $ updateTick shgif
    return $ set canvas (Just newC) $ updateTick shgif
    where
        lastTimeStamp = maximum $ map fst (shgif^.shgifData)
        -- https://docs.unity3d.com/ja/2019.2/ScriptReference/Mathf.Repeat.html
        repeat max val | val < 0   = max
                       
-- | Update 'Shgif''s internal tick state, which will affect frame rendering.  
--
-- As 'updateShgif' has type `Shgif -> IO Shgif`, it can be called inside brick's 'Brick.EventM' monad.
--
updateShgif :: Shgif -> IO Shgif
updateShgif shgif = do
    let updateTick = set currentTick (repeat lastTimeStamp $ (shgif^.currentTick) + 1)
    newC <- shgifToCanvas $ updateTick shgif
    return $ set canvas (Just newC) $ updateTick shgif
    where
        lastTimeStamp = maximum $ map fst (shgif^.shgifData)
        -- https://docs.unity3d.com/ja/2019.2/ScriptReference/Mathf.Repeat.html
        repeat max val | max < val = 0
                       | otherwise = val


-- | Update 'Shgif''s internal tick state to make it closer to given tick
updateShgifTo :: Int -> Shgif -> IO Shgif
updateShgifTo tick shgif  = do
    let tickToAdd = case (shgif^.currentTick) `compare` tick of
                        LT -> 1
                        EQ -> 0
                        GT -> -1
    newC <- shgifToCanvas (shgif&currentTick+~tickToAdd)
    return $ set canvas (Just newC) $ over currentTick (+ tickToAdd) shgif
