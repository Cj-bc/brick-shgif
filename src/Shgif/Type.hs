{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Shgif.Type where

import GHC.Generics
import Control.Lens


-- | format  of shgif file
data Format = Page -- ^ list data as list of String
            | Plot -- ^ list data based on its coordinate

data Shgif = Shgif { _title :: String
                   , _author :: String
                   , _format :: Format
                   , _width :: Int
                   , _heigh :: Int
                   , _data :: [(Float, [String])]  -- ^ [(Time, String to write)]
                    } deriving (Generic)

mkLenses ''Shgif

instance FromJSON Shgif



