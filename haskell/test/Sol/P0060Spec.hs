module Sol.P0060Spec (spec) where

import Test.Hspec

import Sol.P0060 (compute)

spec :: Spec
spec = do
    describe "Problem 60" $ do
        it "four primes" $ do
            compute 4 `shouldBe` "792"

        it "five primes" $ do
            compute 5 `shouldBe` "26033"
