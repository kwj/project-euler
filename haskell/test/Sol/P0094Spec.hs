module Sol.P0094Spec (spec) where

import Test.Hspec
import Sol.P0094 (compute)

spec :: Spec
spec = do
    describe "Problem 94" $ do
        it "<= 1000000000" $ do
            compute 1_000_000_000 `shouldBe` "518408346"
