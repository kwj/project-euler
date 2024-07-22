module Sol.P0010Spec (spec) where

import Test.Hspec

import Sol.P0010 (compute)

spec :: Spec
spec = do
    describe "Problem 10" $ do
        it "below 10" $ do
            compute 10 `shouldBe` "17"

        it "below 2_000_000" $ do
            compute 2_000_000 `shouldBe` "142913828922"
