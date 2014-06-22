{-# LANGUAGE OverloadedStrings #-}

module GooglePlus.Activity
  ( getPublicActivitiesList
  , getPublicActivity
  , ActivitiesList(..)
  , Activity(..)
  , convertToOriginalContent
  , getOriginalContent
  , Verb
  , ActivityObject(..))
where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Control.Applicative ( (<$>), (<*>), (*>), (<*), (<|>), many )
import Control.Monad (mzero)

import Data.Attoparsec.Text
import Data.Aeson
import Network.HTTP.Conduit

endpointRoot :: String
endpointRoot = "https://www.googleapis.com/plus/v1"

getPublicActivitiesList :: ApiKey -> UserID -> IO BSL.ByteString
getPublicActivitiesList apiKey userId =
  simpleHttp $ withKey apiKey $ endpointRoot ++ "/people/" ++ userId ++ "/activities/public"

getPublicActivity :: ApiKey -> ActivityID -> IO BSL.ByteString
getPublicActivity aKey aId =
  simpleHttp $ withKey aKey $ endpointRoot ++ "/activities/" ++ aId

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
  parseJSON _ = mzero

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
  parseJSON _ = mzero

data ActivityObject = ActivityObject
  { objectType :: Text
  , content :: Text
  , originalContent :: Maybe Text }
  deriving ( Read, Show )

getOriginalContent :: ActivityObject -> Either Text Text
getOriginalContent ao = maybe (Left $ content ao) Right (originalContent ao)

instance FromJSON ActivityObject where
  parseJSON (Object o) = do
    t <- o .: "objectType"
    c <- o .: "content"
    oc <- o .: "originalContent" <|> return (convertToOriginalContent c)
    return $ ActivityObject t c oc
  parseJSON _ = mzero

convertToOriginalContent :: Text -> Maybe Text
convertToOriginalContent t = T.concat <$> result
  where
    result = maybeResult $ feedEmpty $ parse (many replacer <* endOfInput) t
    replacer =
          beginB
      <|> endB
      <|> beginI
      <|> endI
      <|> beginS
      <|> endS
      <|> beginA
      <|> endA
      <|> br
      <|> andAmp
      <|> andQuot
      <|> and39
      <|> andLt
      <|> andGt
      <|> takeWhile1 (not . (`elem` "<&"))
    feedEmpty = (flip feed) ""

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
beginS = replace "<del>" "-"
endS :: Parser Text
endS = replace "</del>" "-"

beginA :: Parser Text
beginA = string "<a" >> many (notChar '>') >> char '>' >> return ""
endA :: Parser Text
endA = replace "</a>" ""

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
