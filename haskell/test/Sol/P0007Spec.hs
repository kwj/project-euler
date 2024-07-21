module Sol.P0007Spec (spec) where

import Test.Hspec
import Sol.P0007 (compute)

spec :: Spec
spec = do
    describe "Problem 7" $ do
        it "6-th prime" $ do
            compute 6 `shouldBe` "13"

        it "10001-th prime" $ do
            compute 10_001 `shouldBe` "104743"
