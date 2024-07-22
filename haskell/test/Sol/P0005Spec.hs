module Sol.P0005Spec (spec) where

import Test.Hspec

import Sol.P0005 (compute)

spec :: Spec
spec = do
    describe "Problem 5" $ do
        it "up to 10" $ do
            compute 10 `shouldBe` "2520"

        it "up to 20" $ do
            compute 20 `shouldBe` "232792560"
