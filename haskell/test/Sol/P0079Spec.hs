module Sol.P0079Spec (spec) where

import Test.Hspec

import Sol.P0079 (compute)

spec :: Spec
spec = do
    describe "Problem 79" $ do
        it "keylog.txt" $ do
            compute `shouldBe` "73162890"
