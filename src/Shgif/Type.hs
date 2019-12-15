{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Shgif.Type (
    Format(..), Shgif(..)
    , shgifToCanvas
) where

import GHC.Generics (Generic)
import Control.Lens (makeLenses)
import Data.HashMap.Lazy ((!))
import qualified Data.Vector as V
import Data.Text (unpack)
import Data.Maybe (fromMaybe)
import Data.Yaml (FromJSON(..), withObject, (.:), Object(..), withArray
                 , Parser(..), Value(..))
import Tart.Canvas (Canvas, canvasFromText)

-- | format  of shgif file
data Format = Page -- ^ list data as list of String
            | Plot -- ^ list data based on its coordinate
            deriving (Generic, Show)

type TimeStamp = (Int, [String])

timeStamp :: Int -> [String] -> TimeStamp
timeStamp time ds = (time, ds)

-- | The main datatype that holds Shgif data
data Shgif = Shgif { _title     :: String
                   , _author    :: String
                   , _format    :: Format
                   , _width     :: Int
                   , _heigh     :: Int
                   , _currentTick :: Int
                   , _shgifData :: [TimeStamp]  -- ^ [(Time, String to write)]
                    } deriving (Show)

makeLenses ''Shgif

instance FromJSON Format


instance FromJSON Shgif where
  parseJSON = withObject "Shgif" $ \v -> Shgif
        <$> v .: "title"
        <*> v .: "author"
        <*> v .: "format"
        <*> v .: "width"
        <*> v .: "heigh"
        <*> return 0
        <*> parseFrame (v ! "data")

parseFrame :: Value -> Parser [TimeStamp]
parseFrame = withArray "data" $ \a -> sequence $ V.toList $ V.map parseTimeStamp a


parseTimeStamp :: Value -> Parser TimeStamp
parseTimeStamp = withObject "Frame" $ \f -> timeStamp
                    <$> f .: "timestamp"
                    <*> parseContents (f ! "contents")

parseContents :: Value -> Parser [String]
parseContents = withArray "contents" $ \a -> return $ V.toList $ V.map (toStr) a
    where
        toStr (String a) = unpack a


-- | Convert Shgif into Tart.Canvas datatype
-- This function only determine which frame to render, and pass it to @canvasFromText@
shgifToCanvas :: Shgif -> IO Canvas
shgifToCanvas (Shgif _ _ _ w h tick ds) =
    let currentFrame t = fromMaybe (currentFrame (t-1)) $ lookup t ds
        frame = currentFrame tick :: [String]
    in canvasFromText $ unlines frame


