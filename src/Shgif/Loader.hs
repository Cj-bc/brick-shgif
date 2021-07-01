{-|
Module      : Shgif.Loader
Description : Collection of Loaders for Shgif
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

This module provides __Loader__s for Shgif.

'Loader' is a function that'll load Shgif from Specified files/Values.

As Those are just function, no type-class-ish thing is provided.
-}
module Shgif.Loader (
    -- * from original format
      fromFile
    , fromFiles

    -- * 'Tart' related
    , fromCanvas
    , fromCanvasWithMeta
    , fromTartFile
) where
import Control.Lens ((&), (+~), (^.))
import Shgif.Type (Shgif(..), Format(..))
import Shgif.Type.Internal (TimeStamp, addInitialCanvas)
import Data.Yaml (ParseException, decodeFileEither)
import Data.Either (isLeft)
import qualified Data.Text as T
import Tart.Format (TartFile(..), sortedCanvases)
import Tart.Canvas (Canvas, canvasSize, prettyPrintCanvas
                   )

defaultTimeStampInterval = 100



-- | Load 'Shgif' data from Yaml file
fromFile :: FilePath -> IO (Either ParseException Shgif)
fromFile n = do
    sgf <- (decodeFileEither n :: IO (Either ParseException Shgif))
    case sgf of
      Left e -> return $ Left e
      Right shgif -> do
        sgf' <- addInitialCanvas shgif
        return $ Right sgf'


-- | Load list of 'Shgif's from list of Yaml file
fromFiles :: [FilePath] -> IO (Either [ParseException] [Shgif])
fromFiles xs = do
  results <- sequence $ map fromFile xs :: IO [Either ParseException Shgif]
  if (containsLeft results)
    then return $ Left  $ caughtExceptions results
    else return $ Right $ map fromRight results
  where
    containsLeft rs     = True `elem` map isLeft rs
    fromLeft (Left e)   = e
    fromRight (Right a) = a
    caughtExceptions rs = map fromLeft $ filter isLeft rs


-- | Create 'Shgif' from 'Tart.Canvas.Canvas'es without any meta value
--
-- If you can implement meta values, use 'fromCanvasWithMeta' instead.
--
-- The first argument specify the tick for each frame.
--
-- If 'Nothing', use 'defaultTimeStampInterval'
fromCanvas :: Maybe [Int] -> [Canvas] -> Shgif
fromCanvas (Just ts)  cs = fromCanvasWithMeta "" "" ts cs
fromCanvas Nothing cs = fromCanvasWithMeta "" "" defaultTimestamps cs
    where
        defaultTimestamps = take (length cs) $ 0: interval 0 defaultTimeStampInterval
        interval orig i = orig + i: interval (orig + i) i

-- | Create 'Shgif' from 'Tart.Format.TartFile'
fromTartFile :: Maybe [Int] -> TartFile -> Shgif
fromTartFile ix tartFile = let cs  = sortedCanvases (tartFileCanvasOrder tartFile) (tartFileCanvasList tartFile)
                           in        fromCanvas Nothing $ cs ++ [last cs]


-- | Create 'Shgif' from 'Tart.Canvas.Canvas' with meta value
--
-- This only support 'Page' format, because 'Tart.Canvas.Canvas' is Bitmap image.
--
-- For __Not__ 'updateTo' use cases:
-- Make sure to __duplicate the last Canvas__ so that it'll show up for more than 1 tick.
--
-- (If you don't, The last frame only appear for 1 tick. In most case, it's the same as invisible)
fromCanvasWithMeta :: String -> String -> [Int] -> [Canvas] -> Shgif
fromCanvasWithMeta title author timestamps cs = Shgif title author Page w h 0 convertedData Nothing
  where
    (w, h) = foldr1 (\(x,y ) (x', y') -> (max x x', max y y')) whList
    whList = fmap (canvasSize) cs
    convertedData :: [TimeStamp]
    convertedData = zip timestamps $ fmap (T.lines . prettyPrintCanvas False . pure) cs


