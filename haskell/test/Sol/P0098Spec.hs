module Sol.P0098Spec (spec) where

import Test.Hspec
import Sol.P0098 (compute)

spec :: Spec
spec = do
    describe "Problem 98" $ do
        it "anagramic squares" $ do
            compute `shouldBe` "18769"
