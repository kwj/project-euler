module Sol.P0035Spec (spec) where

import Test.Hspec
import Sol.P0035 (compute)

spec :: Spec
spec = do
    describe "Problem 35" $ do
        it "below 100" $ do
            compute 100 `shouldBe` "13"

        it "below 1000000" $ do
            compute 1_000_000 `shouldBe` "55"
