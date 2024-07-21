module Sol.P0066Spec (spec) where

import Test.Hspec
import Sol.P0066 (compute)

spec :: Spec
spec = do
    describe "Problem 66" $ do
        it "D <= 7" $ do
            compute 7 `shouldBe` "5"

        it "D <= 1000" $ do
            compute 1_000 `shouldBe` "661"
