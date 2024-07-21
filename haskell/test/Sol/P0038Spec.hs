module Sol.P0038Spec (spec) where

import Test.Hspec
import Sol.P0038 (compute)

spec :: Spec
spec = do
    describe "Problem 38" $ do
        it "maximum pandigital multiplies" $ do
            compute `shouldBe` "932718654"
