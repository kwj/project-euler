module Sol.P0045Spec (spec) where

import Test.Hspec

import Sol.P0045 (compute)

spec :: Spec
spec = do
    describe "Problem 45" $ do
        it "first number" $ do
            compute 1 `shouldBe` "1"

        it "second number" $ do
            compute 2 `shouldBe` "40755"

        it "third number" $ do
            compute 3 `shouldBe` "1533776805"
