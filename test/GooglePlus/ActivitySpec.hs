module GooglePlus.ActivitySpec where

import GooglePlus.Activity

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ( (==>) )

main :: IO ()
main = hspec $ do
  describe "convertToOriginalContent" $ do
    prop "convert <b> to aseterisk" $ \ s1 s2 s3 ->
      hasNoLt s1 && hasNoLt s2 && hasNoLt s3 ==>
        let content = s1 ++ "<b>" ++ s2 ++ "</b>" ++ s3
            originalContent = s1 ++ "*" ++ s2 ++ "*" ++ s3
        in
        convertToOriginalContent content == originalContent

hasNoLt :: String -> Bool
hasNoLt s = not $ '<' `elem` s
