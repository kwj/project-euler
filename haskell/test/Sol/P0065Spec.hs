module Sol.P0065Spec (spec) where

import Test.Hspec

import Sol.P0065 (compute)

spec :: Spec
spec = do
    describe "Problem 65" $ do
        it "10th convergent" $ do
            compute 10 `shouldBe` "17"

        it "100th convergent" $ do
            compute 100 `shouldBe` "272"
