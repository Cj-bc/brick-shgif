{-|
Module      : Brick.Extensions.Shgif.Widgets
Description : Brick extension for "Shgif"
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

This module provides "Brick"'s widget for "Shgif" data.
-}
module Brick.Extensions.Shgif.Widgets
(
-- * Widget for Shgif
  shgif
, shgifs

-- * Widget for Container
, container

-- * other
, canvas
) where

import Control.Lens ((^.), view)
import Data.Maybe (fromJust, isNothing)
import Shgif.Type (Shgif, shgifToCanvas, Container, rendered)
import qualified Shgif.Type as T
import qualified Graphics.Vty as Vty
import Brick.Types (Widget(..), Size(..))
import Brick.Widgets.Core
import Brick.Widgets.Border (border)
import Tart.Canvas (Canvas(..), canvasLayersToImage)

-- | Widget to show 'Shgif' file
shgif :: Shgif -> Widget n
shgif s | isNothing (s^.T.canvas) = raw $ Vty.charFill Vty.defAttr ' ' (s^.T.width) (s^.T.height)
        | otherwise               = canvas [fromJust $ s^.T.canvas]


-- | Widget to show multiple 'Shgif'
--
-- This will __merge__ all Shgifs into One Widget.
--
-- __First one is the top__.
shgifs :: [Shgif] -> Widget n
shgifs = canvas . mconcat . fmap (maybe [] pure . view T.canvas)


-- | Widget to show '[Tart.Canvas]'
canvas :: [Canvas] -> Widget n
canvas c = raw $ canvasLayersToImage c


-- | Widget to show 'Container'
container :: Container -> Widget n
container c = case c^.rendered of
                Nothing -> emptyWidget
                Just c' -> canvas [c']
