module Sol.P0083Spec (spec) where

import Test.Hspec
import Sol.P0083 (compute)

spec :: Spec
spec = do
    describe "Problem 83" $ do
        it "matrix.txt" $ do
            compute `shouldBe` "425185"
