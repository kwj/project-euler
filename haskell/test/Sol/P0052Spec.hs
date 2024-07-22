module Sol.P0052Spec (spec) where

import Test.Hspec

import Sol.P0052 (compute)

spec :: Spec
spec = do
    describe "Problem 52" $ do
        it "permuted multiples" $ do
            compute `shouldBe` "142857"
