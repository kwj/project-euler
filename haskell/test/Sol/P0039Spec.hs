module Sol.P0039Spec (spec) where

import Test.Hspec

import Sol.P0039 (compute)

spec :: Spec
spec = do
    describe "Problem 39" $ do
        it "perimeteor <= 1000" $ do
            compute 1_000 `shouldBe` "840"
