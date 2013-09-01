{-# LANGUAGE OverloadedStrings #-}

module Settings
  ( Settings(..)
  , loadSettings ) where

import Data.Text as Text
import Data.Text ()
import Data.ByteString as ByteString
import Control.Applicative
  ( (<$>)
  , (<*>) )

import Data.Yaml

data Settings =
  Settings
  { userId :: String
  , apiKey :: String } deriving Show

instance FromJSON Settings where
  parseJSON (Object o) = do
    userId <- show <$> ( o .: "userId" :: Parser Integer )
    apiKey <- o .: "apiKey"
    return $ Settings userId apiKey

loadSettings :: FilePath -> IO (Either String Settings)
loadSettings fp = decodeEither <$> ByteString.readFile fp
