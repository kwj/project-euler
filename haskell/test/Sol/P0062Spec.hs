module Sol.P0062Spec (spec) where

import Test.Hspec

import Sol.P0062 (compute)

spec :: Spec
spec = do
    describe "Problem 62" $ do
        it "three permutations of its digits are cube" $ do
            compute 3 `shouldBe` "41063625"

        it "five permutations of its digits are cube" $ do
            compute 5 `shouldBe` "127035954683"
