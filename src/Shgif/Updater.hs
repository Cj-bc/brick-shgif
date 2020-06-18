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
    , setShgifTickTo
) where
import Shgif.Type.Internal
import Control.Lens (over, set, (+~), (&), (^.), (.~))
getLastTimeStamp :: Shgif -> Int
getLastTimeStamp = maximum . map fst . view shgifData

type Updater = Shgif -> IO Shgif

-- | Update 'Shgif''s internal tick state, which will affect frame rendering.
--
-- As 'updateShgif' has type `Shgif -> IO Shgif`, it can be called inside 'Brick.EventM' monad.
--
-- This function __won't loop__ gif.
--
-- Use this if you want to show animation only once.
updateShgifNoLoop :: Updater
updateShgifNoLoop shgif@(Shgif t a f w h tick ds c) = updateShgifCore updateTick
    where
        updateTick | tick <= lastTimeStamp = over currentTick (+ 1)
                   | otherwise             = id
        lastTimeStamp = getLastTimeStamp shgif


-- | Update 'Shgif''s internal tick state, which will affect frame rendering.  
--
-- As 'updateShgif' has type `Shgif -> IO Shgif`, it can be called inside brick's 'Brick.EventM' monad.
--
-- This function reverse and __won't loop__ gif.
--
-- Use this if you want to show reversed animation for only once.
updateShgifReversedNoLoop :: Updater
updateShgifReversedNoLoop shgif = updateShgifCore updateTick
    where
        updateTick | 0 < (shgif^.currentTick) = over currentTick (subtract 1)
                   | otherwise                = id


-- | Update 'Shgif''s internal tick state, which will affect frame rendering.  
--
-- As 'updateShgif' has type `Shgif -> IO Shgif`, it can be called inside brick's 'Brick.EventM' monad.
--
-- This function reverse gif.
--
-- Use this if you want to show reversed animation.
updateShgifReversed :: Updater
updateShgifReversed shgif = updateShgifCore updateTick
    where
        lastTimeStamp = getLasTimestamp shgif
        -- https://docs.unity3d.com/ja/2019.2/ScriptReference/Mathf.Repeat.html
        repeat max val | val < 0   = max
        updateTick = set currentTick (repeat lastTimeStamp $ (shgif^.currentTick) - 1)

-- | Update 'Shgif''s internal tick state, which will affect frame rendering.  
--
-- As 'updateShgif' has type `Shgif -> IO Shgif`, it can be called inside brick's 'Brick.EventM' monad.
--
updateShgif :: Updater
updateShgif shgif = updateShgifCore updateTick
    where
        lastTimeStamp = getLasTimestamp shgif
        -- https://docs.unity3d.com/ja/2019.2/ScriptReference/Mathf.Repeat.html
        repeat max val | max < val = 0
                       | otherwise = val
        updateTick = set currentTick (repeat lastTimeStamp $ (shgif^.currentTick) + 1)


-- | Update 'Shgif''s internal tick state to make it closer to given tick
updateShgifTo :: Int -> Updater
updateShgifTo tick shgif  = updateShgifCore (currentTick+~tickToAdd)
    where
        tickToAdd = case (shgif^.currentTick) `compare` tick of
                        LT -> 1
                        EQ -> 0
                        GT -> -1


-- | Set 'Shgif''s internal tick state to given tick.
setShgifTickTo :: Int -> Updater
setShgifTickTo tick shgif = updateShgifCore (currentTick~.tick)


-- | Core functionality of 'updateShgif'
--
-- This is expected to be used inside of other 'updateShgif' function.
-- This apply 'updateTick' function to Shgif and return updated 'Shgif'
updateShgifCore :: (Shgif -> Shgif) -> IO Shgif
updateShgifCore updateTick = do
    newC <- shgifToCanvas $ updateTick shgif
    return $ set canvas (Just newC) $ updateTick shgif


