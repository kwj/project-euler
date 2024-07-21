module Sol.P0067Spec (spec) where

import Test.Hspec
import Sol.P0067 (compute)

spec :: Spec
spec = do
    describe "Problem 67" $ do
        it "triangle.txt" $ do
            compute `shouldBe` "7273"
