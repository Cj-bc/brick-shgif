module Brick.Extensions.Shgif.Widgets where

import Control.Lens ((^.))
import Data.Maybe (fromJust, isNothing)
import Shgif.Type (Shgif, shgifToCanvas)
import qualified Shgif.Type as T
import qualified Graphics.Vty as Vty
import Brick.Types (Widget(..), Size(..))
import Brick.Widgets.Core
import Brick.Widgets.Border (border)
import Tart.Canvas (Canvas(..), canvasLayersToImage)

shgif :: Shgif -> Widget n
shgif s | isNothing (s^.T.canvas) = raw $ Vty.charFill Vty.defAttr ' ' (s^.T.width) (s^.T.heigh) -- Is order correct? (width, heigh) ?
        |otherwise               = canvas $ fromJust $ s^.T.canvas

canvas :: Canvas -> Widget n
canvas c = border $ raw $ canvasLayersToImage [c]
