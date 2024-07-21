module Sol.P0002Spec (spec) where

import Test.Hspec
import Sol.P0002 (compute)

spec :: Spec
spec = do
    describe "Problem 2" $ do
        it "even fib numbers <= 100" $ do
            compute 100 `shouldBe` "44"

        it "even fib numbers <= 4_000_000" $ do
            compute 4_000_000 `shouldBe` "4613732"
