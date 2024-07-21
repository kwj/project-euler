module Sol.P0043Spec (spec) where

import Test.Hspec
import Sol.P0043 (compute)

spec :: Spec
spec = do
    describe "Problem 43" $ do
        it "sub-string divisibility" $ do
            compute `shouldBe` "16695334890"
