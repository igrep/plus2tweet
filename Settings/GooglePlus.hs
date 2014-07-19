{-# LANGUAGE OverloadedStrings #-}

module Settings.GooglePlus
  ( Settings(..)
  ) where

import Control.Applicative
  ( (<$>)
  )
import Control.Monad

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
  parseJSON _ = mzero
