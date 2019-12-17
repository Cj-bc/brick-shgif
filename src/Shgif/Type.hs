{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Shgif.Type (
    Format(..), Shgif(..)
    , shgifToCanvas, updateShgif, addInitialCanvas
    , canvas, width, height
) where

import GHC.Generics (Generic)
import Control.Lens (makeLenses, (.~), (^.))
import Data.HashMap.Lazy ((!))
import qualified Data.Vector as V
import Data.Text (unpack)
import Data.Maybe (fromMaybe)
import Data.Yaml (FromJSON(..), withObject, (.:), Object(..), withArray
                 , Parser(..), Value(..))
import Tart.Canvas (Canvas, canvasFromText, newCanvas)

-- | format  of shgif file
-- Currently only Page is suported
-- TODO: Support other format
data Format = Page -- ^ list data as list of String
            | Plot -- ^ list data based on its coordinate
            deriving (Generic, Show)

type TimeStamp = (Int, [String])

-- | The main datatype that holds Shgif data
data Shgif = Shgif { _title     :: String
                   , _author    :: String
                   , _format    :: Format
                   , _width     :: Int
                   , _height    :: Int
                   , _currentTick :: Int
                   , _shgifData :: [TimeStamp]  -- ^ [(Time, String to write)]
                   , _canvas :: Maybe Canvas
                    }

makeLenses ''Shgif

instance FromJSON Format


-- instance FromJSON Shgif {{{
instance FromJSON Shgif where
  parseJSON = withObject "Shgif" $ \v -> Shgif
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
parseContents = withArray "contents" $ \a -> return $ V.toList $ V.map (toStr) a
    where
        toStr (String a) = unpack a
-- }}}


-- | Convert Shgif into Tart.Canvas datatype
-- This function only determine which frame to render, and pass it to @canvasFromText@
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



-- | Add initialized @Canvas@
--
-- Shgif object is generated by parsing yaml, which isn't in 'IO'.
-- So it can't execute @newCanvas@ to give Shgif canvas.
-- Instead, I give it canvas here.
addInitialCanvas :: Shgif -> IO Shgif
addInitialCanvas sgf = do
    newC <- newCanvas (sgf^.width, sgf^.height) -- XXXX: is it correct order? (width, height)
    return $ canvas .~ (Just newC) $ sgf

updateShgif :: Shgif -> IO Shgif
updateShgif shgif@(Shgif t a f w h tick ds c) = do
    let tick' = repeat lastTimeStamp $ tick + 1
    newC <- shgifToCanvas $ Shgif t a f w h tick' ds c
    return $ Shgif t a f w h tick' ds (Just newC)
    where
        lastTimeStamp = maximum $ map fst ds
        -- https://docs.unity3d.com/ja/2019.2/ScriptReference/Mathf.Repeat.html
        repeat max val | max < val = 0
                       | otherwise = val
