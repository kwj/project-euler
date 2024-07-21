module Sol.P0058Spec (spec) where

import Test.Hspec
import Sol.P0058 (compute)

spec :: Spec
spec = do
    describe "Problem 58" $ do
        it "spiral primes" $ do
            compute `shouldBe` "26241"
