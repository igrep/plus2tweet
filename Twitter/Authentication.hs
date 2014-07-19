{-# LANGUAGE OverloadedStrings #-}

{-
Most code of this file is copied from one of the samples of Takahiro Himura's twitter-conduit.
https://github.com/himura/twitter-conduit/blob/4e0d58b5d7da3cee1b52995dfe4be8b98d29c970/sample/oauth_pin.hs

Copyright (c)2011-2014, Takahiro Himura

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}


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
