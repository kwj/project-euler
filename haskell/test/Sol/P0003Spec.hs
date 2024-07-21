module Sol.P0003Spec (spec) where

import Test.Hspec
import Sol.P0003 (compute)

spec :: Spec
spec = do
    describe "Problem 3" $ do
        it "number: 13195" $ do
            compute 13_195 `shouldBe` "29"

        it "number: 600851475143" $ do
            compute 600_851_475_143 `shouldBe` "6857"
