module Sol.P0006Spec (spec) where

import Test.Hspec

import Sol.P0006 (compute)

spec :: Spec
spec = do
    describe "Problem 6" $ do
        it "up to 10" $ do
            compute 10 `shouldBe` "2640"

        it "up to 100" $ do
            compute 100 `shouldBe` "25164150"
