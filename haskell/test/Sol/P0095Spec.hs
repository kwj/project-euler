module Sol.P0095Spec (spec) where

import Test.Hspec
import Sol.P0095 (compute)

spec :: Spec
spec = do
    describe "Problem 95" $ do
        it "<= 1000000" $ do
            compute 1_000_000 `shouldBe` "14316"
