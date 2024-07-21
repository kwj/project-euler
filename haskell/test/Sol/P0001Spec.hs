module Sol.P0001Spec (spec) where

import Test.Hspec
import Sol.P0001 (compute)

spec :: Spec
spec = do
    describe "Problem 1" $ do
        it "below 10" $ do
            compute 10 `shouldBe` "23"

        it "below 1_000" $ do
            compute 1_000 `shouldBe` "233168"
