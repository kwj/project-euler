module Sol.P0064Spec (spec) where

import Test.Hspec

import Sol.P0064 (compute)

spec :: Spec
spec = do
    describe "Problem 64" $ do
        it "N <= 13" $ do
            compute 13 `shouldBe` "4"

        it "N <= 10000" $ do
            compute 10_000 `shouldBe` "1322"
