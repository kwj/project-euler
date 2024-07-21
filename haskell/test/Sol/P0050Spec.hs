module Sol.P0050Spec (spec) where

import Test.Hspec
import Sol.P0050 (compute)

spec :: Spec
spec = do
    describe "Problem 50" $ do
        it "under 100" $ do
            compute 100 `shouldBe` "41"

        it "under 500" $ do
            compute 500 `shouldBe` "499"

        it "under 1000" $ do
            compute 1_000 `shouldBe` "953"

        it "under 10000" $ do
            compute 10_000 `shouldBe` "9521"

        it "under 1000000" $ do
            compute 1_000_000 `shouldBe` "997651"
