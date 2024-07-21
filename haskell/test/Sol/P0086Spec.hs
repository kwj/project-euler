module Sol.P0086Spec (spec) where

import Test.Hspec
import Sol.P0086 (compute)

spec :: Spec
spec = do
    describe "Problem 86" $ do
        it "first exceeds 1975" $ do
            compute 1_975 `shouldBe` "100"

        it "first exceeds one million" $ do
            compute 1_000_000 `shouldBe` "1818"
