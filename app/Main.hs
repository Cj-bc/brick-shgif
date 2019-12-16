module Main where

import Data.Yaml (decodeFileEither, ParseException)
import System.Exit (exitFailure)
import Brick
import Brick.Extensions.Shgif.Events

import Shgif.Type

main :: IO ()
main = do
    -- Read Shgif data from File
    sgf <- case (decodeFileEither "filename" :: Either ParseException Shgif) of
                -- Complete initializing Shgif by adding @Canvas@
                Right y -> return $ addInitialCanvas y
                Left e  -> putStrLn $ show e >> exitFailure

    -- Include sgf into your 'State'

    finalState <- mainWithTick Nothing 1000 (App here) (State here)
