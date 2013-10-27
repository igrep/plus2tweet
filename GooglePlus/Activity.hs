{-# LANGUAGE OverloadedStrings #-}

module GooglePlus.Activity
  ( getPublicActivitiesList
  , getPublicActivity
  , ActivitiesList(..)
  , Activity(..)
  , convertToOriginalContent
  , Verb
  , ActivityObject(..))
where

import Data.Text (Text)

import Data.Attoparsec.Text

import Data.ByteString.Lazy as BSL
import Data.Time.Clock (UTCTime)
import Control.Applicative ( (<$>), (<*>), (*>) )

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
  } deriving Show

instance FromJSON ActivitiesList where
  parseJSON (Object o) = ActivitiesList <$>
    o .: "nextPageToken" <*>
    o .: "updated"       <*>
    o .: "items"

data Activity = Activity
  { activityId :: Text
  , activityUrl :: Text
  , verb :: Verb
  , published :: UTCTime
  , activityObject :: ActivityObject }
  deriving Show

instance FromJSON Activity where
  parseJSON (Object o) = Activity <$>
    o .: "id"        <*>
    o .: "url"       <*>
    o .: "verb"      <*>
    o .: "published" <*>
    o .: "object"

data ActivityObject = ActivityObject
  { objectType :: Text
  , content :: Text
  , originalContent :: Maybe Text }
  deriving ( Read, Show )

instance FromJSON ActivityObject where
  parseJSON (Object o) = ActivityObject <$> o .: "objectType" <*> o .: "content" <*> o .:? "originalContent"

convertToOriginalContent :: Text -> Text
convertToOriginalContent = undefined

replace :: Text -> Text -> Parser Text
replace s1 s2 = string s1 *> return s2

beginB :: Parser Text
beginB = replace "<b>" "*"
endB :: Parser Text
endB = replace "</b>" "*"

beginI :: Parser Text
beginI = replace "<i>" "_"
endI :: Parser Text
endI = replace "</i>" "_"

beginS :: Parser Text
beginS = replace "<s>" "-"
endS :: Parser Text
endS = replace "</s>" "-"

br :: Parser Text
br = replace "<br />" "\n"

andAmp :: Parser Text
andAmp = replace "&amp;" "&"

andQuot :: Parser Text
andQuot = replace "&quot;" "\""

and39 :: Parser Text
and39 = replace "&#39;" "'"

andLt :: Parser Text
andLt = replace "&lt;" "<"

andGt :: Parser Text
andGt = replace "&gt;" ">"

type Verb = Text

withKey :: ApiKey -> String -> String
withKey apiKey url = url ++ "?key=" ++ apiKey
