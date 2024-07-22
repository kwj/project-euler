module Sol.P0100Spec (spec) where

import Test.Hspec

import Sol.P0100 (compute)

spec :: Spec
spec = do
    describe "Problem 100" $ do
        it "over 10^12" $ do
            compute 1_000_000_000_000 `shouldBe` "756872327473"
