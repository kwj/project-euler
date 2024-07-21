module Sol.P0033Spec (spec) where

import Test.Hspec
import Sol.P0033 (compute)

spec :: Spec
spec = do
    describe "Problem 33" $ do
        it "digit canceling fractions" $ do
            compute `shouldBe` "100"
