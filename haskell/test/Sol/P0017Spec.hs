module Sol.P0017Spec (spec) where

import Test.Hspec
import Sol.P0017 (compute)

spec :: Spec
spec = do
    describe "Problem 17" $ do
        it "1 to 5" $ do
            compute 5 `shouldBe` "19"

        it "1 to 1000" $ do
            compute 1_000 `shouldBe` "21124"
