module Brick.Extensions.Shgif.Widgets where

import Control.Lens ((^.), view)
import Data.Maybe (fromJust, isNothing)
import Shgif.Type (Shgif, shgifToCanvas)
import qualified Shgif.Type as T
import qualified Graphics.Vty as Vty
import Brick.Types (Widget(..), Size(..))
import Brick.Widgets.Core
import Brick.Widgets.Border (border)
import Tart.Canvas (Canvas(..), canvasLayersToImage)

-- | Widget to show 'Shgif' file
shgif :: Shgif -> Widget n
shgif s | isNothing (s^.T.canvas) = raw $ Vty.charFill Vty.defAttr ' ' (s^.T.width) (s^.T.height) -- Is order correct? (width, height) ?
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
