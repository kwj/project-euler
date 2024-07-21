module Sol.P0069Spec (spec) where

import Test.Hspec
import Sol.P0069 (compute)

spec :: Spec
spec = do
    describe "Problem 69" $ do
        it "n <= 10" $ do
            compute 10 `shouldBe` "6"

        it "n <= 1000000" $ do
            compute 1_000_000 `shouldBe` "510510"
