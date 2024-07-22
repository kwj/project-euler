module Sol.P0087Spec (spec) where

import Test.Hspec

import Sol.P0087 (compute)

spec :: Spec
spec = do
    describe "Problem 87" $ do
        it "under 50" $ do
            compute 50 `shouldBe` "4"

        it "under 50000000" $ do
            compute 50_000_000 `shouldBe` "1097343"
