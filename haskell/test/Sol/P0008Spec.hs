module Sol.P0008Spec (spec) where

import Test.Hspec
import Sol.P0008 (compute)

spec :: Spec
spec = do
    describe "Problem 8" $ do
        it "consecutive length is 4" $ do
            compute 4 `shouldBe` "5832"

        it "consecutive length is 13" $ do
            compute 13 `shouldBe` "23514624000"
