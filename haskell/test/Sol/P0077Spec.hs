module Sol.P0077Spec (spec) where

import Test.Hspec
import Sol.P0077 (compute)

spec :: Spec
spec = do
    describe "Problem 77" $ do
        it "over four different ways" $ do
            compute 4 `shouldBe` "10"

        it "over five thousand different ways" $ do
            compute 5_000 `shouldBe` "71"
