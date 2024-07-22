module Sol.P0014Spec (spec) where

import Test.Hspec

import Sol.P0014 (compute)

spec :: Spec
spec = do
    describe "Problem 14" $ do
        it "under 1_000_000" $ do
            compute 1_000_000 `shouldBe` "837799"
