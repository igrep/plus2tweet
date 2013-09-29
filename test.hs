module Main where

import GooglePlus.Activity
import Settings

import Data.Aeson ( eitherDecode )

import qualified Data.ByteString.Lazy as BSL
import System.Environment ( getArgs )
import Control.Applicative ( (<$>) )

main :: IO ()
main = do
  ( cmd : args ) <- getArgs
  exec cmd args
  where
    exec "dl" = tryDownload
    exec "parse" = tryParseJSON
    exec other = error $ "Undifiend command: " ++ other

tryDownload :: [FilePath] -> IO ()
tryDownload args = do
  Right ( Settings uId aKey ) <- loadSettings $ head args
  getPublicActivitiesList aKey uId >>= BSL.putStr

tryParseJSON :: [FilePath] -> IO ()
tryParseJSON args =
  print =<<
    ( eitherDecode <$> ( BSL.readFile $ head args ) :: IO ( Either String ActivitiesList ) )
