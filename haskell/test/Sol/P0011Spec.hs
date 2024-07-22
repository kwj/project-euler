module Sol.P0011Spec (spec) where

import Test.Hspec

import Sol.P0011 (compute)

spec :: Spec
spec = do
    describe "Problem 11" $ do
        it "consecutive length is 4" $ do
            compute 4 `shouldBe` "70600674"
