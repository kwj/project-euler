module Sol.P0032Spec (spec) where

import Test.Hspec

import Sol.P0032 (compute)

spec :: Spec
spec = do
    describe "Problem 32" $ do
        it "sum of pandigital products" $ do
            compute `shouldBe` "45228"
