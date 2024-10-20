module Sol.P0061Spec (spec) where

import Test.Hspec

import Sol.P0061 (compute)

spec :: Spec
spec = do
    describe "Problem 61" $ do
        it "Triangular and Square numbers" $ do
            compute 4 `shouldBe` "8181"

        it "Triangular, Square and Pentagonal numbers" $ do
            compute 5 `shouldBe` "19291"

        it "Triangular, Square, ... and Octagonal numbers" $ do
            compute 8 `shouldBe` "28684"
