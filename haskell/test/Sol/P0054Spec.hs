module Sol.P0054Spec (spec) where

import Test.Hspec
import Sol.P0054 (compute)

spec :: Spec
spec = do
    describe "Problem 54" $ do
        it "poker.txt" $ do
            compute `shouldBe` "376"
