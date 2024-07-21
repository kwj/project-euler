module Sol.P0072Spec (spec) where

import Test.Hspec
import Sol.P0072 (compute)

spec :: Spec
spec = do
    describe "Problem 72" $ do
        it "d <= 8" $ do
            compute 8 `shouldBe` "21"

        it "d <= 1000000" $ do
            compute 1_000_000 `shouldBe` "303963552391"
