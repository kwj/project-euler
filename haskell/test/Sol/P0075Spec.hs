module Sol.P0075Spec (spec) where

import Test.Hspec
import Sol.P0075 (compute)

spec :: Spec
spec = do
    describe "Problem 75" $ do
        it "L <= 48" $ do
            compute 48 `shouldBe` "6"

        it "L <= 1500000" $ do
            compute 1_500_000 `shouldBe` "161667"
