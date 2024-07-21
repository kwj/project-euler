module Sol.P0076Spec (spec) where

import Test.Hspec
import Sol.P0076 (compute)

spec :: Spec
spec = do
    describe "Problem 76" $ do
        it "total: 5" $ do
            compute 5 [1 .. 4] `shouldBe` "6"

        it "total: 100" $ do
            compute 100 [1 .. 99] `shouldBe` "190569291"
