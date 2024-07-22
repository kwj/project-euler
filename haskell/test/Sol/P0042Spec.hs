module Sol.P0042Spec (spec) where

import Test.Hspec

import Sol.P0042 (compute)

spec :: Spec
spec = do
    describe "Problem 42" $ do
        it "words.txt" $ do
            compute `shouldBe` "162"
