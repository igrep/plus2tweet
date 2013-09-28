{-# LANGUAGE OverloadedStrings #-}

module GooglePlus.Activity
  ( getPublicActivitiesList
  , getPublicActivity
  , Activity(..)
  , ActivitiesList(..)
  , Verb(..) )
where

import Data.Text as Text
import Data.Text ()

import Data.ByteString.Lazy as BSL
import Data.Time.Clock (UTCTime)

import Data.Aeson
import Network.HTTP.Conduit

getPublicActivitiesList :: ApiKey -> UserID -> IO BSL.ByteString
getPublicActivitiesList apiKey userId =
  simpleHttp $ withKey apiKey $ "https://www.googleapis.com/plus/v1/people/" ++ userId ++ "/activities/public"

getPublicActivity :: ApiKey -> ActivityID -> IO BSL.ByteString
getPublicActivity aKey aId =
  simpleHttp $ withKey aKey $ "https://www.googleapis.com/plus/v1/activities/" ++ aId

type ApiKey = String
type UserID = String
type ActivityID = String

data ActivitiesList = ActivitiesList
  { nextPageToken :: Text
  , updated :: UTCTime
  , items :: [Activity]
  }

instance FromJSON ActivitiesList where
  parseJSON (Object o) = do
    aNextPageToken <- o .: "nextPageToken"
    aUpdated       <- o .: "updated"
    aItems         <- o .: "items"
    return $ ActivitiesList aNextPageToken aUpdated aItems


data Activity = Activity
  { activityId :: Text
  , activityUrl :: Text
  , verb :: Verb
  , published :: UTCTime
  , originalContent :: Text }
  deriving Show

instance FromJSON Activity where
  parseJSON (Object o) = do
    aId              <- o .: "id"
    aUrl             <- o .: "url"
    aVerb            <- o .: "verb"
    aPublished       <- o .: "published"
    aObject          <- o .: "object"
    aOriginalContent <- aObject .: "originalContent"
    return $ Activity aId aUrl aVerb aPublished aOriginalContent

data Verb = Post | Share deriving (Read, Show)

instance FromJSON Verb where
  parseJSON (String s) = return $ read $ Text.unpack s

withKey :: ApiKey -> String -> String
withKey apiKey url = url ++ "?key=" ++ apiKey
