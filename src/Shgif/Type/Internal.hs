{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_HADDOCK hide #-}
{-|
Module      : Shgif.Type.Internal
Description : some Type definitnions for Internal use export
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

This module aims to hide some 'Only internal use' functions (like Lens)
from 'Shgif.Type'.
-}
module Shgif.Type.Internal where
import Control.Lens (makeLenses, (.~), (^.), (&), (+~), Lens, set, view, over, _2, each)
import Data.Yaml (FromJSON(..), withObject, (.:), Object(..), withArray
                 , withText
                 , Parser(..), Value(..)
                 , decodeFileEither)
import Data.HashMap.Lazy ((!))
import qualified Data.Vector as V
import Data.Text (unpack, splitOn, Text)
import Data.Maybe (fromMaybe)
import Linear.V2 (V2(..))

import GHC.Generics (Generic)
import Tart.Canvas (Canvas, canvasFromText, newCanvas, canvasGetPixel, canvasSetPixel, canvasSize)

version = (1, 0, 0)


-- | Mark Type as "Updatable"
--
-- Instances are able to use 'Updater'
class Updatable a where
    -- | The core for all 'Updater'
    -- Implement this, and you can use all 'Updater' defined in 'Shgif.Updater'
    --
    -- First argument is a function which takes current tick and return updated tick.
    -- Because of the instances that hold multiple ticks, I keep it to update only one tick
    update :: (Int -> Int) -> a -> IO a

    -- | Get the last timestamp in 'a'
    getLastTimeStamp :: a -> Int


-- | Format  of shgif file
--
-- Currently only Page is suported
--
-- TODO: Support other format
data Format = Page -- ^ list data as list of String
            | Plot -- ^ list data based on its coordinate
            deriving (Generic, Show)

-- | TimeStamp is used to represent one _frame_
type TimeStamp = (Int, [String])

-- | 'Shgif.Updater'
--
-- Updater takes 'Updatable' value and return updated result.
type Updater = forall a. Updatable a => a -> IO a

-- | The main datatype that holds Shgif data
data Shgif = Shgif { _title     :: String
                   , _author    :: String
                   , _format    :: Format
                   , _width     :: Int
                   , _height    :: Int
                   , _currentTick :: Int
                   , _shgifData :: [TimeStamp]  -- ^ [(Time, String to write)]
                   , _canvas :: Maybe Canvas    -- ^ the 'Tart.Canvas' which will be rendered
                    }

makeLenses ''Shgif

instance FromJSON Format


-- instance FromJSON Shgif {{{
instance FromJSON Shgif where
  parseJSON = do
      sgf <- parseJSON'
      validateVersion sgf
    where
        -- | Validate Shgif format version and fail if it's not supported.
        -- If it's supported, do nothing
        --
        -- Supported version is:
        --
        -- - The same major version
        -- - The same or smaller minor version
        validateVersion sgf = withObject "Shgif" $ \v -> do
                            let getMajorV (a, _, _) = a
                                getMinorV (_, a, _) = a
                                parseVersion        = withText "version" $ \v -> do
                                                        let versions = map (read . unpack) $ splitOn (".") v
                                                        if (length versions /= 3)
                                                          then fail "Unsupported version format"
                                                          else return (versions !! 0, versions !! 1, versions !! 2)
                                condition lib file  = (getMajorV lib == getMajorV file)
                                                      && (getMinorV lib >= getMinorV file)

                            usedVersion <- parseVersion (v ! "version")
                            if (condition version usedVersion)
                              then sgf
                              else fail . unlines $ ["Shgif format version mismatch. Major version should be the same."
                                                    , "Supported version: " ++ (show version)
                                                    , "Used version: " ++ (show usedVersion)
                                                    ]

        parseJSON' = withObject "Shgif" $ \v -> Shgif
                        <$> v .: "title"
                        <*> v .: "author"
                        <*> v .: "format"
                        <*> v .: "width"
                        <*> v .: "height"
                        <*> return 0
                        <*> parseFrame (v ! "data")
                        <*> return Nothing

parseFrame :: Value -> Parser [TimeStamp]
parseFrame = withArray "data" $ \a -> sequence $ V.toList $ V.map parseTimeStamp a


parseTimeStamp :: Value -> Parser TimeStamp
parseTimeStamp = withObject "Frame" $ \f -> timeStamp
                    <$> f .: "timestamp"
                    <*> parseContents (f ! "contents")
    where
        timeStamp :: Int -> [String] -> TimeStamp
        timeStamp time ds = (time, ds)

parseContents :: Value -> Parser [String]
parseContents = withText "Contents" (return . tail . lines . unpack)
-- }}}

-- | Container
--
-- Save multiple 'Shgif's with coordinate offset.
--
-- Object of Container:
--
-- - Move multiple 'Shgif's synchronicity, keeping relative position
--
-- - Move multiple 'Shgif's independently, keeping relative position
--
-- Solution for each:
--
-- - Use the same Tick to move all 'Shgif's
--
-- - Let user to apply different 'Updater' to each 'Shgif'
--
-- コンテナでやりたいこと:
--
-- - 複数のShgifの位置関係を保ったまま、同時に動かしたい
--
-- - 複数のShgifの位置関係を保ったまま、バラバラに動かしたい
--
-- それぞれのソリューション:
--
-- - Shgifに同じTickを適用することで、同時に動かす
--
-- - 各Shgifへ 'Updater' を指定して適用できるようにする
data Container = Container { _syncedTick :: Maybe Int           -- ^ synced Tick value. If 'Nothing', it won't sync
                           , _shgifs     :: [((Int, Int), Shgif)]   -- ^ pair of (Offset, Shgif).
                           , _rendered   :: Canvas              -- ^ Rendered 'Canvas'
                           }
makeLenses ''Container

instance Updatable Container where
    -- |
    --
    -- If Tick is synced, update it and apply it to all 'Shgif's.
    -- If not, call update function for each 'Shgif'
    --
    -- __This doesn't support independent update__
    --
    -- Tickが同期されていたら、同期されたTickを更新しそれを各 'Shgif' に反映する。
    -- されてないかったら、各 'Shgif' にupdateを呼び出す
    --
    -- __バラバラに動かす処理はしないことに注意__
    --
    update updateTick c = case (c^.syncedTick) of
                Just t  -> do
                    let t'               = updateTick t
                        updateSyncedTick = syncedTick (const . return . Just $ t')
                        updateEachShgif  = (shgifs . each . _2) (update (const t'))
                    updateSyncedTick c >>= updateEachShgif >>= updateRendered
                Nothing -> (shgifs . each . _2) (update updateTick) c >>= updateRendered
        where
            updateRendered c' = rendered (const . mergeToBigCanvas . view shgifs $ c') c'

    -- | We use the latest timestamp in all all shgif data for Container's last Time stamp.
    -- By doing this,
    --
    -- If we use the 'smallest' number as last time stamp,
    -- some frames could be cut off.
    --
    -- If we don't decide last time stamp for the Container, each 'Shgif' in Container will be updated independently,
    -- resulting in non-synced animation.
    getLastTimeStamp = maximum . map (getLastTimeStamp . snd) . view shgifs


-- Helper functions {{{


-- | Convert 'Shgif' into 'Tart.Canvas' datatype
-- This function only determine which frame to render, and pass it to 'canvasFromText'
shgifToCanvas :: Shgif -> IO Canvas
shgifToCanvas (Shgif _ _ _ w h tick ds _) = canvasFromText $ unlines $ map (addWidthPadding w) $ addHeightPadding  h frame
    where
        currentFrame t = fromMaybe (currentFrame (t-1)) $ lookup t ds
        frame :: [String]
        frame = currentFrame tick

        addWidthPadding :: Int -> String -> String
        addWidthPadding _ [] = []
        addWidthPadding req_width x | length x < req_width = x ++ replicate (req_width - (length x)) ' '
                                    | otherwise            = x

        addHeightPadding :: Int -> [String] -> [String]
        addHeightPadding req_height xs | length xs < req_height = xs ++ replicate (req_height - length xs) ""
                                       | otherwise              = xs



-- | Add initialized 'Canvas'
--
-- Shgif object is generated by parsing yaml, which isn't in 'IO'.
-- So it can't execute 'newCanvas' to give Shgif canvas.
-- Instead, I give it canvas here.
addInitialCanvas :: Shgif -> IO Shgif
addInitialCanvas sgf = do
    newC <- newCanvas (sgf^.width, sgf^.height) -- XXXX: is it correct order? (width, height)
    newC' <- shgifToCanvas $ sgf&canvas.~(Just newC)
    return $ sgf&canvas.~(Just newC')


instance Updatable Shgif where
    update updateTick shgif = do
        let updated = over currentTick updateTick shgif
        newC <- shgifToCanvas updated
        return $ set canvas (Just newC) updated
    getLastTimeStamp = maximum . map fst . view shgifData

-- Those functions below are borrowed from:
--  https://github.com/Cj-bc/faclig/blob/master/src/Graphics/Asciiart/Faclig/Types.hs#L106-L138

-- | Render Canvas into other canvas
plotToCanvas :: (Int, Int) -> Canvas -> Canvas -> IO Canvas
plotToCanvas (dw, dh) bc c = do
    let (w, h) = canvasSize c
    write [(w', h') | w' <- [0..w-1], h' <- [0..h-1]] bc
    where
        write :: [(Int, Int)] -> Canvas -> IO Canvas
        write [] bc'         = return bc'
        write ((w, h):x) bc' = do
            let (ch, attr) = canvasGetPixel c (w, h)
            case ch of
                ' ' -> write x bc
                _   -> do
                  newC <- canvasSetPixel bc (w + dw, h + dh) ch attr
                  write x newC



-- | Merge and render all Shgifs into one Canvas
mergeToBigCanvas :: [((Int, Int), Shgif)] -> IO Canvas
mergeToBigCanvas ss = do
    emptyCanvas <- newCanvas (w, h)
    write ss emptyCanvas
    where
        w = maximum $ fmap (\((w',_), s) -> s^.width + w') ss
        h = maximum $ fmap (\((_,h'), s) -> s^.height + h') ss

        -- | Write Canvases one after another
        write :: [((Int, Int), Shgif)] -> Canvas -> IO Canvas
        write [] c         = return c
        write ((p,s):x) bc = do
            shgifC <- shgifToCanvas s
            newC <- plotToCanvas p bc shgifC
            write x newC
-- }}}
