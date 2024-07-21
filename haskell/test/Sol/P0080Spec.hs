module Sol.P0080Spec (spec) where

import Test.Hspec
import Sol.P0080 (compute)

spec :: Spec
spec = do
    describe "Problem 80" $ do
        it "up to 2" $ do
            compute 2 100 `shouldBe` "475"

        it "up to 100" $ do
            compute 100 100 `shouldBe` "40886"
