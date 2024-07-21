module Sol.P0012Spec (spec) where

import Test.Hspec
import Sol.P0012 (compute)

spec :: Spec
spec = do
    describe "Problem 12" $ do
        it "threshold: 5" $ do
            compute 5 `shouldBe` "28"

        it "threshold: 500" $ do
            compute 500 `shouldBe` "76576500"
