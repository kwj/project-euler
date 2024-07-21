module Sol.P0056Spec (spec) where

import Test.Hspec
import Sol.P0056 (compute)

spec :: Spec
spec = do
    describe "Problem 56" $ do
        it "powerful digit sum" $ do
            compute `shouldBe` "972"
