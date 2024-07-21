module Sol.P0092Spec (spec) where

import Test.Hspec
import Sol.P0092 (compute)

spec :: Spec
spec = do
    describe "Problem 92" $ do
        it "under 10" $ do
            compute 10 `shouldBe` "7"

        it "under 10000000" $ do
            compute 10_000_000 `shouldBe` "8581146"
