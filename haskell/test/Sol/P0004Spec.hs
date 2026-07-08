module Sol.P0004Spec (spec) where

import Test.Hspec

import Sol.P0004 (compute)

spec :: Spec
spec = do
    describe "Problem 4" $ do
        it "1-digit numbers" $ do
            compute 1 `shouldBe` "9"

        it "2-digit numbers" $ do
            compute 2 `shouldBe` "9009"

        it "3-digit numbers" $ do
            compute 3 `shouldBe` "906609"
