{-# LANGUAGE OverloadedStrings #-}

module GooglePlus.Activity
  ( getPublicActivitiesList
  , getPublicActivity
  , Activity(..)
  , Verb(..) )
where

import Data.Text as Text
import Data.Text ()
import Control.Applicative
  ( (<$>)
  , (<*>) )

import Data.ByteString.Lazy as BSL
import Data.Time.Clock (UTCTime)

import Data.Aeson
import Network.HTTP.Conduit

getPublicActivitiesList :: ApiKey -> UserID -> IO BSL.ByteString
getPublicActivitiesList apiKey userId =
  simpleHttp $ withKey apiKey $ "https://www.googleapis.com/plus/v1/people/" ++ userId ++ "/activities/public"

getPublicActivity :: ApiKey -> ActivityID -> IO BSL.ByteString
getPublicActivity apiKey activityId =
  simpleHttp $ withKey apiKey $ "https://www.googleapis.com/plus/v1/activities/" ++ activityId

type ApiKey = String
type UserID = String
type ActivityID = String

data Activity = Activity
  { activityId :: Text
  , activityUrl :: Text
  , verb :: Verb
  , published :: UTCTime
  , originalContent :: Text }
  deriving Show

instance FromJSON Activity where
  parseJSON (Object o) = do
    activityId      <- o .: "id"
    activityUrl     <- o .: "url"
    verb            <- o .: "verb"
    published       <- o .: "published"
    obj             <- o .: "object"
    originalContent <- obj .: "originalContent"
    return $ Activity activityId activityUrl verb published originalContent

data Verb = Post | Share deriving (Read, Show)

instance FromJSON Verb where
  parseJSON (String s) = return $ read $ Text.unpack s

withKey :: ApiKey -> String -> String
withKey apiKey url = url ++ "?key=" ++ apiKey
