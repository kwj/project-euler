module Sol.P0082Spec (spec) where

import Test.Hspec

import Sol.P0082 (compute)

spec :: Spec
spec = do
    describe "Problem 82" $ do
        it "matrix.txt" $ do
            compute `shouldBe` "260324"
