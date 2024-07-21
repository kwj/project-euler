module Sol.P0048Spec (spec) where

import Test.Hspec
import Sol.P0048 (compute)

spec :: Spec
spec = do
    describe "Problem 48" $ do
        it "max power is 10" $ do
            compute 10 `shouldBe` "0405071317"

        it "max power is 1000" $ do
            compute 1_000 `shouldBe` "9110846700"
