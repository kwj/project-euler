module Sol.P0022Spec (spec) where

import Test.Hspec
import Sol.P0022 (compute)

spec :: Spec
spec = do
    describe "Problem 22" $ do
        it "names.txt" $ do
            compute `shouldBe` "871198282"
