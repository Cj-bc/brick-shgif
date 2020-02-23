{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Shgif.Type (
    Format(..)
    , Shgif(..)
    , TimeStamp
    , shgifToCanvas
    , canvas, width, height
) where

import Shgif.Type.Internal
-- Config {{{
defaultTimeStampInterval = 100
--- }}}

