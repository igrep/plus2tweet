{-# LANGUAGE OverloadedStrings #-}

module Settings.Twitter
  ( Settings(..)
  ) where

import Control.Monad
import Control.Applicative ((<$>), (<*>))

import Data.Yaml

data Settings =
  Settings
  { consumerKey :: String
  , consumerSecret :: String
  , accessKey :: String
  , accessToken :: String
  } deriving Show

instance FromJSON Settings where
  parseJSON (Object o) =
    Settings <$>
          o .:  "consumerKey"
      <*> o .:  "consumerSecret"
      <*> o .:? "accessKey"   .!= ""
      <*> o .:? "accessToken" .!= ""
  parseJSON _ = mzero
