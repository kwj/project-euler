module Sol.P0036Spec (spec) where

import Test.Hspec
import Sol.P0036 (compute)

spec :: Spec
spec = do
    describe "Problem 36" $ do
        it "under 1000000" $ do
            compute 1_000_000 `shouldBe` "872187"
