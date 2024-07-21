module Sol.P0021Spec (spec) where

import Test.Hspec
import Sol.P0021 (compute)

spec :: Spec
spec = do
    describe "Problem 21" $ do
        it "under 10000" $ do
            compute 10_000 `shouldBe` "31626"
