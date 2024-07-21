module Sol.P0044Spec (spec) where

import Test.Hspec
import Sol.P0044 (compute)

spec :: Spec
spec = do
    describe "Problem 44" $ do
        it "pentagon numbers" $ do
            compute `shouldBe` "5482660"
