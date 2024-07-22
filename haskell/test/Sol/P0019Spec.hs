module Sol.P0019Spec (spec) where

import Test.Hspec

import Sol.P0019 (compute)

spec :: Spec
spec = do
    describe "Problem 19" $ do
        it "Jan 1901 - Nov 2000" $ do
            compute `shouldBe` "171"
