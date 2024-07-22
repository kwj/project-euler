module Sol.P0063Spec (spec) where

import Test.Hspec

import Sol.P0063 (compute)

spec :: Spec
spec = do
    describe "Problem 63" $ do
        it "powerful digit counts" $ do
            compute `shouldBe` "49"
