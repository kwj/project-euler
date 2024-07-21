module Sol.P0025Spec (spec) where

import Test.Hspec
import Sol.P0025 (compute)

spec :: Spec
spec = do
    describe "Problem 25" $ do
        it "3 digits" $ do
            compute 3 `shouldBe` "12"

        it "1000 digits" $ do
            compute 1_000 `shouldBe` "4782"
