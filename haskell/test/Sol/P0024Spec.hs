module Sol.P0024Spec (spec) where

import Test.Hspec
import Sol.P0024 (compute)

spec :: Spec
spec = do
    describe "Problem 24" $ do
        it "1_000_000th number" $ do
            compute 1_000_000 `shouldBe` "2783915460"
