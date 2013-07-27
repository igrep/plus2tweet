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
  { userId :: Text
  , apiKey :: Text } deriving Show

instance FromJSON Settings where
  parseJSON (Object o) =
    Settings <$> o .: "userId" <*> o .: "apiKey"

loadSettings :: FilePath -> IO (Either String Settings)
loadSettings fp = decodeEither <$> ByteString.readFile fp
