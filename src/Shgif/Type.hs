{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Shgif.Type
Description : Collection of Types for Shgif
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

This module provides common Types for Shgif.
-}
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

