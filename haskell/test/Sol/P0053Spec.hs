module Sol.P0053Spec (spec) where

import Test.Hspec

import Sol.P0053 (compute)

spec :: Spec
spec = do
    describe "Problem 53" $ do
        it "n <= 23" $ do
            compute 23 1_000_000 `shouldBe` "4"

        it "n <= 100" $ do
            compute 100 1_000_000 `shouldBe` "4075"
