module Sol.P0059Spec (spec) where

import Test.Hspec
import Sol.P0059 (compute)

spec :: Spec
spec = do
    describe "Problem 59" $ do
        it "cipher.txt" $ do
            compute `shouldBe` "129448"
