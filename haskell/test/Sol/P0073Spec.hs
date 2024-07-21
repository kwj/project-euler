module Sol.P0073Spec (spec) where

import Test.Hspec
import Sol.P0073 (compute)

spec :: Spec
spec = do
    describe "Problem 73" $ do
        it "d <= 8" $ do
            compute 8 `shouldBe` "3"

        it "d <= 12000" $ do
            compute 12_000 `shouldBe` "7295372"
