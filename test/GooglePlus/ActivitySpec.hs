{-# LANGUAGE OverloadedStrings #-}

module GooglePlus.ActivitySpec where

import GooglePlus.Activity ( convertToOriginalContent )

import qualified Data.Text as Text
import Data.Text ()

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ( (==>) )

spec :: Spec
spec = do
  describe "convertToOriginalContent" $ do
    prop "convert <b> to aseterisk" $ \ s1 s2 s3 ->
      hasNoLtAmp s1 && hasNoLtAmp s2 && hasNoLtAmp s3 ==>
        let content = s1 ++ "<b>" ++ s2 ++ "</b>" ++ s3
            originalContent = s1 ++ "*" ++ s2 ++ "*" ++ s3
        in
        -- DO NOT try to get rid of String: Text is not an Arbitrary so far.
        convertToOriginalContent ( Text.pack content ) `shouldBe` (Just $ Text.pack originalContent)

    it "strips <a> around a URL" $
      convertToOriginalContent "<a href=\"http://www.example.com\">http://www.example.com</a> is an example URL"
        `shouldBe` (Just "http://www.example.com is an example URL")

    it "strips <a> around a hashtag" $
      convertToOriginalContent "This is a test <a rel=\"nofollow\" class=\"ot-hashtag\" href=\"https://plus.google.com/s/%23thisisatest\">#thisisatest</a>"
        `shouldBe` (Just "This is a test #thisisatest")

hasNoLtAmp :: String -> Bool
hasNoLtAmp = not . any (`elem` "<&")
