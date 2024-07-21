module Sol.P0018Spec (spec) where

import Test.Hspec
import Sol.P0018 (compute)

spec :: Spec
spec = do
    describe "Problem 18" $ do
        it "small triangle" $ do
            compute `shouldBe` "1074"
