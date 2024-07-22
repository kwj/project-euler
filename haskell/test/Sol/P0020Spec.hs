module Sol.P0020Spec (spec) where

import Test.Hspec

import Sol.P0020 (compute)

spec :: Spec
spec = do
    describe "Problem 20" $ do
        it "10!" $ do
            compute 10 `shouldBe` "27"

        it "100!" $ do
            compute 100 `shouldBe` "648"
