module Sol.P0030Spec (spec) where

import Test.Hspec

import Sol.P0030 (compute)

spec :: Spec
spec = do
    describe "Problem 30" $ do
        it "fourth powers" $ do
            compute 4 `shouldBe` "19316"

        it "fifth powers" $ do
            compute 5 `shouldBe` "443839"
