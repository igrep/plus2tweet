{-# LANGUAGE OverloadedStrings #-}

module Settings
  ( Settings(..)
  , loadSettings ) where

import Data.ByteString as ByteString
import Control.Applicative
  ( (<$>)
  )

import Data.Yaml

data Settings =
  Settings
  { userId :: String
  , apiKey :: String } deriving Show

instance FromJSON Settings where
  parseJSON (Object o) = do
    uId <- show <$> ( o .: "userId" :: Parser Integer )
    aKey <- o .: "apiKey"
    return $ Settings uId aKey

loadSettings :: FilePath -> IO (Either String Settings)
loadSettings fp = decodeEither <$> ByteString.readFile fp
