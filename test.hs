module Main where

import GooglePlus.Activity
import Settings

import Data.Aeson ( eitherDecode, FromJSON )
import qualified Text.XML.HXT.Core as HXT

import qualified Data.ByteString.Lazy as BSL
import System.Environment ( getArgs )
import Control.Applicative ( (<$>) )
import qualified Data.Text.IO as TIO
import Data.Text ( Text )

main :: IO ()
main = do
  ( cmd : args ) <- getArgs
  exec cmd args
  where
    exec "dl" = tryDownload
    exec "parse-j" = tryParseJSON
    exec "parse-c" = tryParseContent
    exec "e-content" = tryExtractContentWith TIO.putStrLn
    exec "e-content-show" = tryExtractContentWith print
    exec other = error $ "Undifiend command: " ++ other

tryDownload :: [FilePath] -> IO ()
tryDownload args = do
  Right ( Settings uId aKey ) <- loadSettings $ head args
  getPublicActivitiesList aKey uId >>= BSL.putStr

tryParseJSON :: [FilePath] -> IO ()
tryParseJSON args =
  print =<< loadActivitiesList ( head args )

tryParseContent :: [String] -> IO ()
tryParseContent args = do
  -- runLA ( hread ) "<root>Test</root>A"
  doc <- HXT.runX . HXT.xshow $ HXT.readString [HXT.withParseHTML HXT.yes] $ head args
  print doc

tryExtractContentWith :: ( Text -> IO() ) -> [FilePath] -> IO ()
tryExtractContentWith printer args =
  mapM_ ( printer . content . activityObject ) =<< items <$> loadActivitiesList ( head args )

loadActivitiesList :: FilePath -> IO ActivitiesList
loadActivitiesList jsonPath = unsafeDecode <$> BSL.readFile jsonPath

unsafeDecode :: FromJSON a => BSL.ByteString -> a
unsafeDecode = leftError . eitherDecode

leftError :: Either String a -> a
leftError = either error id
