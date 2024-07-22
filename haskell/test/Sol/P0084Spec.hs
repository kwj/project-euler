module Sol.P0084Spec (spec) where

import Test.Hspec

import Sol.P0084 (compute)

spec :: Spec
spec = do
    describe "Problem 84" $ do
        it "4-sided dice" $ do
            compute 4 `shouldBe` "101524"

        it "6-sided dice" $ do
            compute 6 `shouldBe` "102400"
