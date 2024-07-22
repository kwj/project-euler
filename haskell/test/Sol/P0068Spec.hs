module Sol.P0068Spec (spec) where

import Test.Hspec

import Sol.P0068 (compute)

spec :: Spec
spec = do
    describe "Problem 68" $ do
        it "3-gon ring" $ do
            compute 3 `shouldBe` "432621513"

        it "5-gon ring" $ do
            compute 5 `shouldBe` "6531031914842725"
