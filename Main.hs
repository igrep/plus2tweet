module Main where

import GooglePlus.Activity
import Settings

import Data.ByteString.Lazy as BSL

main :: IO ()
main = do
  Right ( Settings userId apiKey ) <- loadSettings "igrep/settings.yml"
  getPublicActivitiesList apiKey userId >>= BSL.putStr
