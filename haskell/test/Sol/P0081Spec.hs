module Sol.P0081Spec (spec) where

import Test.Hspec

import Sol.P0081 (compute)

spec :: Spec
spec = do
    describe "Problem 81" $ do
        it "matrix.txt" $ do
            compute `shouldBe` "427337"
