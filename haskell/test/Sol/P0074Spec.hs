module Sol.P0074Spec (spec) where

import Test.Hspec
import Sol.P0074 (compute)

spec :: Spec
spec = do
    describe "Problem 74" $ do
        it "under one milion (10^6)" $ do
            compute 6 `shouldBe` "402"
