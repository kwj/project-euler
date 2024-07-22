module Sol.P0023Spec (spec) where

import Test.Hspec

import Sol.P0023 (compute)

spec :: Spec
spec = do
    describe "Problem 23" $ do
        it "up to 28123" $ do
            compute 28_123 `shouldBe` "4179871"
