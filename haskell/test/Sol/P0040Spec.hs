module Sol.P0040Spec (spec) where

import Test.Hspec
import Sol.P0040 (compute)

spec :: Spec
spec = do
    describe "Problem 40" $ do
        it "producut of d1, d10, d100, d1000, d10000, d100000 and d1000000" $ do
            compute `shouldBe` "210"
