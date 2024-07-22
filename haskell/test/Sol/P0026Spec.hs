module Sol.P0026Spec (spec) where

import Test.Hspec

import Sol.P0026 (compute)

spec :: Spec
spec = do
    describe "Problem 26" $ do
        it "under 10" $ do
            compute 10 `shouldBe` "7"

        it "under 300" $ do
            compute 300 `shouldBe` "289"

        it "under 1000" $ do
            compute 1_000 `shouldBe` "983"
