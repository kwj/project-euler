module Sol.P0029Spec (spec) where

import Test.Hspec
import Sol.P0029 (compute)

spec :: Spec
spec = do
    describe "Problem 29" $ do
        it "2 <= a <= 100 and 2 <= b <= 100" $ do
            compute 100 `shouldBe` "9183"
