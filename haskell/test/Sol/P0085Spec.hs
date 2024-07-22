module Sol.P0085Spec (spec) where

import Test.Hspec

import Sol.P0085 (compute)

spec :: Spec
spec = do
    describe "Problem 85" $ do
        it "about 2000000" $ do
            compute 2_000_000 `shouldBe` "2772"
