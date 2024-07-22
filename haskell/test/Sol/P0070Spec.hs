module Sol.P0070Spec (spec) where

import Test.Hspec

import Sol.P0070 (compute)

spec :: Spec
spec = do
    describe "Problem 70" $ do
        it "totient permutation (1 < n < 10^7)" $ do
            compute `shouldBe` "8319823"
