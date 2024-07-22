module Sol.P0090Spec (spec) where

import Test.Hspec

import Sol.P0090 (compute)

spec :: Spec
spec = do
    describe "Problem 90" $ do
        it "cube digit parts" $ do
            compute `shouldBe` "1217"
