module Sol.P0049Spec (spec) where

import Test.Hspec

import Sol.P0049 (compute)

spec :: Spec
spec = do
    describe "Problem 49" $ do
        it "prime permutations" $ do
            compute `shouldBe` "296962999629"
