module Sol.P0046Spec (spec) where

import Test.Hspec
import Sol.P0046 (compute)

spec :: Spec
spec = do
    describe "Problem 46" $ do
        it "goldbach's other conjecture" $ do
            compute `shouldBe` "5777"
