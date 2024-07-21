module Sol.P0027Spec (spec) where

import Test.Hspec
import Sol.P0027 (compute)

spec :: Spec
spec = do
    describe "Problem 27" $ do
        it "n^2 + an + b, where abs(a) < 1000 and abs(b) <= 1000" $ do
            compute `shouldBe` "-59231"
