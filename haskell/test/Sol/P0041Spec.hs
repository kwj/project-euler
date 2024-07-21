module Sol.P0041Spec (spec) where

import Test.Hspec
import Sol.P0041 (compute)

spec :: Spec
spec = do
    describe "Problem 41" $ do
        it "largest pandigital prime" $ do
            compute `shouldBe` "7652413"
