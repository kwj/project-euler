module Sol.P0009Spec (spec) where

import Test.Hspec

import Sol.P0009 (compute)

spec :: Spec
spec = do
    describe "Problem 9" $ do
        it "perimeter 12 (3 + 4 + 5)" $ do
            compute 12 `shouldBe` "60"

        it "perimeter 36 (9 + 12 + 15)" $ do
            compute 36 `shouldBe` "1620"

        it "perimeter 1000" $ do
            compute 1_000 `shouldBe` "31875000"
