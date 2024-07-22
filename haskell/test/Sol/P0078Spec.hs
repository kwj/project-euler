module Sol.P0078Spec (spec) where

import Test.Hspec

import Sol.P0078 (compute)

spec :: Spec
spec = do
    describe "Problem 78" $ do
        it "n `mod` 7 == 0" $ do
            compute 7 `shouldBe` "5"

        it "n `mod` 10^6 == 0" $ do
            compute 1_000_000 `shouldBe` "55374"
