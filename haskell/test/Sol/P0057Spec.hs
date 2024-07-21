module Sol.P0057Spec (spec) where

import Test.Hspec
import Sol.P0057 (compute)

spec :: Spec
spec = do
    describe "Problem 57" $ do
        it "8 expansions" $ do
            compute 8 `shouldBe` "1"

        it "1000 expansions" $ do
            compute 1_000 `shouldBe` "153"
