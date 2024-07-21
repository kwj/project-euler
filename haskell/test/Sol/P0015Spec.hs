module Sol.P0015Spec (spec) where

import Test.Hspec
import Sol.P0015 (compute)

spec :: Spec
spec = do
    describe "Problem 15" $ do
        it "2 x 2 grid" $ do
            compute 2 2 `shouldBe` "6"

        it "20 x 20 grid" $ do
            compute 20 20 `shouldBe` "137846528820"
