module Sol.P0096Spec (spec) where

import Test.Hspec
import Sol.P0096 (compute)

spec :: Spec
spec = do
    describe "Problem 96" $ do
        it "Sudoku" $ do
            compute `shouldBe` "24702"
