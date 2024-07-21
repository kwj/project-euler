module Sol.P0091Spec (spec) where

import Test.Hspec
import Sol.P0091 (compute)

spec :: Spec
spec = do
    describe "Problem 91" $ do
        it "2 x 2" $ do
            compute 2 2 `shouldBe` "14"

        it "50 x 50" $ do
            compute 50 50 `shouldBe` "14234"
