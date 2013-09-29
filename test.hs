module Main where

import GooglePlus.Activity
import Settings

import Data.Aeson ( eitherDecode )

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
    exec "parse" = tryParseJSON
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

tryExtractContentWith :: ( Text -> IO() ) -> [FilePath] -> IO ()
tryExtractContentWith printer args =
  mapM_ ( printer . content . activityObject ) =<< items <$> loadActivitiesList ( head args )

loadActivitiesList :: FilePath -> IO ActivitiesList
loadActivitiesList jsonPath = unsafeDecode <$> BSL.readFile jsonPath
  where
    unsafeDecode = leftError . eitherDecode
    leftError = either error id
