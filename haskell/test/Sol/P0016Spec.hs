module Sol.P0016Spec (spec) where

import Test.Hspec
import Sol.P0016 (compute)

spec :: Spec
spec = do
    describe "Problem 16" $ do
        it "2^15" $ do
            compute 15 `shouldBe` "26"

        it "2^1000" $ do
            compute 1_000 `shouldBe` "1366"
