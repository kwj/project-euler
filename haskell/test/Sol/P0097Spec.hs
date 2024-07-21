module Sol.P0097Spec (spec) where

import Test.Hspec
import Sol.P0097 (compute)

spec :: Spec
spec = do
    describe "Problem 97" $ do
        it "large non-Mersenne prime" $ do
            compute `shouldBe` "8739992577"
