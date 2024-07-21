module Sol.P0031Spec (spec) where

import Test.Hspec
import Sol.P0031 (compute)

spec :: Spec
spec = do
    describe "Problem 31" $ do
        it "UK's coins" $ do
            compute 200 [1, 2, 5, 10, 20, 50, 100, 200] `shouldBe` "73682"
