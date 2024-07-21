module Sol.P0034Spec (spec) where

import Test.Hspec
import Sol.P0034 (compute)

spec :: Spec
spec = do
    describe "Problem 34" $ do
        it "digit factorials" $ do
            compute `shouldBe` "40730"
