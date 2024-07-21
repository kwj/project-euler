module Sol.P0037Spec (spec) where

import Test.Hspec
import Sol.P0037 (compute)

spec :: Spec
spec = do
    describe "Problem 37" $ do
        it "truncatable primes" $ do
            compute `shouldBe` "748317"
