{-# LANGUAGE OverloadedStrings #-}

import GooglePlus.Activity
import Settings

import Data.Aeson ( eitherDecode, FromJSON )
import qualified Text.XML.HXT.Core as HXT

import qualified Data.ByteString.Lazy as BSL
import System.Environment ( getArgs )
import Control.Applicative ( (<$>) )
import qualified Data.Text.IO as TIO
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Monoid

main :: IO ()
main = do
  ( cmd : args ) <- getArgs
  exec cmd args
  where
    exec "dl" = tryDownload
    exec "parse-j" = tryParseJSON
    exec "parse-c" = tryParseContent
    exec "e-content" = tryExtractWith content TIO.putStrLn
    exec "e-content-show" = tryExtractWith content print
    exec "e-ocontent" = tryExtractWith showOriginalContent TIO.putStrLn
    exec "e-ocontent-show" = tryExtractWith showOriginalContent print
    exec "tw" = tryTweet
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

tryExtractWith :: (ActivityObject -> Text) -> (Text -> IO()) -> [FilePath] -> IO ()
tryExtractWith prop printer args =
  mapM_ ( printer . prop . activityObject ) =<< items <$> loadActivitiesList ( head args )

showOriginalContent :: ActivityObject -> Text
showOriginalContent = formatEither . originalContent

tryTweet :: [String] -> IO ()
tryTweet args = do
  undefined

loadActivitiesList :: FilePath -> IO ActivitiesList
loadActivitiesList jsonPath = unsafeDecode <$> BSL.readFile jsonPath

unsafeDecode :: FromJSON a => BSL.ByteString -> a
unsafeDecode = leftError . eitherDecode

leftError :: Either String a -> a
leftError = either error id

formatEither :: Either String Text -> Text
formatEither (Left s) = T.pack $ "Left: " <> s
formatEither (Right t) = "Right: " <> t
