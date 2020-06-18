{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Lens (makeLenses, (.~), (^.), (&), (+~), Lens, set, view)
import Data.Yaml (FromJSON(..), withObject, (.:), Object(..), withArray
                 , withText
                 , Parser(..), Value(..)
                 , decodeFileEither)
import Data.HashMap.Lazy ((!))
import qualified Data.Vector as V
import Data.Text (unpack, splitOn, Text)
import Data.Maybe (fromMaybe)

import GHC.Generics (Generic)
import Tart.Canvas (Canvas, canvasFromText, newCanvas)

version = (1, 0, 0)


-- | Mark Type as "Updatable"
--
-- Instances are able to use 'Updater'
class Updatable a where
    -- | The core for all 'Updater'
    -- Implement this, and you can use all 'Updater' defined in 'Shgif.Updater'
    update :: (a -> a) -> a -> IO a

    -- | Lens to get tick from 'a'
    getTick :: Lens a a Int Int

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
        newC <- shgifToCanvas $ updateTick shgif
        return $ set canvas (Just newC) $ updateTick shgif
    getTick = currentTick
    getLastTimeStamp = maximum . map fst . view shgifData
