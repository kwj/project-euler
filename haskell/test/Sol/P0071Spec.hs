module Sol.P0071Spec (spec) where

import Test.Hspec
import Sol.P0071 (compute)

spec :: Spec
spec = do
    describe "Problem 71" $ do
        it "d <= 8" $ do
            compute 8 `shouldBe` "2"

        it "d <= 1000000" $ do
            compute 1_000_000 `shouldBe` "428570"
