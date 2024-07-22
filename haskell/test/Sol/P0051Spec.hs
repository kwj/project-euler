module Sol.P0051Spec (spec) where

import Test.Hspec

import Sol.P0051 (compute)

spec :: Spec
spec = do
    describe "Problem 51" $ do
        it "family size 8" $ do
            compute 8 `shouldBe` "121313"
