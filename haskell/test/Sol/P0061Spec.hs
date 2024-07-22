module Sol.P0061Spec (spec) where

import Test.Hspec

import Sol.P0061 (compute)

spec :: Spec
spec = do
    describe "Problem 61" $ do
        it "cyclical figurate numbers" $ do
            compute `shouldBe` "28684"
