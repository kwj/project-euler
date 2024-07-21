module Sol.P0089Spec (spec) where

import Test.Hspec
import Sol.P0089 (compute)

spec :: Spec
spec = do
    describe "Problem 89" $ do
        it "roman.txt" $ do
            compute `shouldBe` "743"
