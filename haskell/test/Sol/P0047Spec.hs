module Sol.P0047Spec (spec) where

import Test.Hspec

import Sol.P0047 (compute)

spec :: Spec
spec = do
    describe "Problem 47" $ do
        it "two prime factors" $ do
            compute 2 `shouldBe` "14"

        it "three prime factors" $ do
            compute 3 `shouldBe` "644"

        it "four prime factors" $ do
            compute 4 `shouldBe` "134043"
