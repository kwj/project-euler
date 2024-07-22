module Sol.P0028Spec (spec) where

import Test.Hspec

import Sol.P0028 (compute)

spec :: Spec
spec = do
    describe "Problem 28" $ do
        it "5 x 5" $ do
            compute 5 `shouldBe` "101"

        it "1001 x 1001" $ do
            compute 1_001 `shouldBe` "669171001"
