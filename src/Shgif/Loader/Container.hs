{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Shgif.Loader.Container
Description : Loaders for 'Container'
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental
-}
module Shgif.Loader.Container (
    fromFile
  , fromFiles

) where
import Control.Monad (forM, when)
import Data.Yaml (FromJSON(..), withObject, withArray, withText
                 , Parser, Value, ParseException
                 , (.:), (.:?)
                 , decodeFileEither
                 )
import Data.HashMap.Lazy ((!))
import Data.Void (Void)
import qualified Data.Vector as V
import qualified Data.Text as T
import Shgif.Type (Shgif, Container(..))
import Shgif.Type.Internal (containerVersion)
import qualified Shgif.Loader as ShgifLoader
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import System.Directory (withCurrentDirectory)
import System.FilePath.Posix (takeDirectory)


type Offset = (Int, Int)
data ContainerFile = ContainerFile { author     :: Maybe T.Text
                                   , title      :: Maybe T.Text
                                   , shgifFiles :: [(Offset, FilePath)]
                                   }


-- instance FromJSON ContainerFile {{{
instance FromJSON ContainerFile where
    parseJSON = do
        ctn <- parseJSON'
        validateVersion ctn
        where
            parseJSON' = withObject "Container" $ \v -> ContainerFile
                <$> v .:? "author"
                <*> v .:? "title"
                <*> parseData (v ! "data")


-- | This is temporary borrowed from Shgif.Type.Internal
validateVersion sgf = withObject "Container" $ \v -> do
                            let getMajorV (a, _, _) = a
                                getMinorV (_, a, _) = a
                                parseVersion :: Value -> Parser (Int, Int, Int)
                                parseVersion = withText "version" $ return . mkTriple . take 3 . map (read . T.unpack) . T.split (== '.')
                                    where
                                        mkTriple (mej:min:patch:_) = (mej, min, patch)
                                condition :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
                                condition lib file  = (getMajorV lib == getMajorV file)
                                                      && (getMinorV lib >= getMinorV file)

                            usedVersion <- parseVersion (v ! "version")
                            if (condition containerVersion usedVersion)
                            then sgf
                            else fail . unlines $ ["Shgif format version mismatch. Major version should be the same."
                                                  , "Supported version: " ++ (show containerVersion)
                                                  , "Used version: " ++ (show usedVersion)
                                                  ]

-- | Parse data section
parseData :: Value -> Parser [(Offset, FilePath)]
parseData = withArray "ContainerShgifList" (sequence . V.toList . V.map parseContainerShgif)
    where
        parseContainerShgif = withObject "ContainerShgif" $ \o -> (,) <$> parseTuple (o ! "offset") <*> o .: "path"

parseTuple :: Read a => Value -> Parser (a, a)
parseTuple = withText "tuple" $ \t -> do
    case M.parse parseTuple' "" t of
        Left e -> fail $ "Invalid Tuple syntax. Megapersec Error: " ++ M.errorBundlePretty e
        Right t' -> return t'

parseTuple' :: (Read a, Read b) => M.Parsec Void T.Text (a, b)
parseTuple' = do
    MC.char '('
    first  <- M.manyTill MC.alphaNumChar (MC.char ',')
    second <- M.manyTill MC.alphaNumChar (MC.char ')')
    return (read first, read second)


-- Borrowed from:
-- https://github.com/Cj-bc/faclig/blob/74db25231e262276d79ba09fc1c23072b7a087a3/src/Graphics/Asciiart/Faclig/Types/Internal.hs#L64-L66

-- }}}

fromFile :: FilePath -> IO (Either ParseException Container)
fromFile fp = do
    container <- (decodeFileEither fp :: IO (Either ParseException ContainerFile))
    case container of
        Left e -> return $ Left e
        Right cf -> withCurrentDirectory (takeDirectory fp) $ do
            shgifs <- forM (shgifFiles cf) $ \(o, sfp) -> do
                          sgf <- ShgifLoader.fromFile sfp
                          case sgf of
                              Left e      -> return $ Left e
                              Right sgf'  -> return $ Right (o, sgf')
            return $ Container Nothing (author cf) (title cf) <$> sequence shgifs <*> return Nothing


fromFiles :: [FilePath] -> IO (Either ParseException [Container])
fromFiles fps = mapM fromFile fps >>= return . sequence
