module Sol.P0088Spec (spec) where

import Test.Hspec

import Sol.P0088 (compute)

spec :: Spec
spec = do
    describe "Problem 88" $ do
        it "2 <= k <= 6" $ do
            compute 6 `shouldBe` "30"

        it "2 <= k <= 12" $ do
            compute 12 `shouldBe` "61"

        it "2 <= k <= 12000" $ do
            compute 12_000 `shouldBe` "7587457"
