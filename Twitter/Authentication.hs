{-# LANGUAGE OverloadedStrings #-}

module Twitter.Authentication
  ( getCredential
  ) where

import qualified Data.ByteString.Char8 as BSC8
import System.IO

import Web.Twitter.Conduit.Monad (twitterOAuth)
import qualified Web.Authenticate.OAuth as OA
import Control.Monad.IO.Class
import Network.HTTP.Conduit

getCredential
  :: String -- consumerKey
  -> String -- consumerSecret
  -> IO OA.Credential
getCredential consumerKey consumerSecret =
  withManager $ \mgr -> do
    cred <- OA.getTemporaryCredential oauth mgr
    pin <- getPIN $ OA.authorizeUrl oauth cred
    OA.getAccessToken oauth (OA.insert "oauth_verifier" pin cred) mgr
  where
    oauth = twitterOAuth
      { OA.oauthConsumerKey = BSC8.pack consumerKey
      , OA.oauthConsumerSecret = BSC8.pack consumerSecret
      }
    getPIN url = liftIO $ do
        putStrLn $ "Open " ++ url ++ " with your browser."
        putStr "> What PIN code did twitter show you? "
        hFlush stdout
        BSC8.getLine
