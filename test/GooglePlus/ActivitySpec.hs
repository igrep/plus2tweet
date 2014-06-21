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
        convertToOriginalContent ( Text.pack content ) `shouldBe` (Right $ Text.pack originalContent)

hasNoLtAmp :: String -> Bool
hasNoLtAmp = not . any (`elem` "<&")
