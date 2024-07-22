module Sol.P0093Spec (spec) where

import Test.Hspec

import Sol.P0093 (compute)

spec :: Spec
spec = do
    describe "Problem 93" $ do
        it "arithmetic expressions" $ do
            compute `shouldBe` "1258"
