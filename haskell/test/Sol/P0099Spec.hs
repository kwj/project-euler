module Sol.P0099Spec (spec) where

import Test.Hspec
import Sol.P0099 (compute)

spec :: Spec
spec = do
    describe "Problem 99" $ do
        it "largest exponential" $ do
            compute `shouldBe` "709"
