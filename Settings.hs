{-# LANGUAGE OverloadedStrings #-}

module Settings
  ( Settings(..)
  , loadSettings
  ) where

import qualified Settings.GooglePlus as GooglePlus
import qualified Settings.Twitter as Twitter

import qualified Data.ByteString as ByteString
import Control.Applicative ((<$>), (<*>))
import Control.Monad

import Data.Yaml

data Settings =
  Settings
  { googlePlusSettings  :: GooglePlus.Settings
  , twitterPlusSettings :: Twitter.Settings
  } deriving Show

instance FromJSON Settings where
  parseJSON (Object o) =
    Settings <$> o .: "GooglePlus"
             <*> o .: "Twitter"
  parseJSON _ = mzero

loadSettings :: FilePath -> IO (Either String Settings)
loadSettings fp = decodeEither <$> ByteString.readFile fp
