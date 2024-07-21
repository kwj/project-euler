module Sol.P0013Spec (spec) where

import Test.Hspec
import Sol.P0013 (compute)

spec :: Spec
spec = do
    describe "Problem 13" $ do
        it "top 10-digit" $ do
            compute 10 `shouldBe` "5537376230"
