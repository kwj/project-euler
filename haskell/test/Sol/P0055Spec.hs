module Sol.P0055Spec (spec) where

import Test.Hspec
import Sol.P0055 (compute)

spec :: Spec
spec = do
    describe "Problem 55" $ do
        it "lychrel numbers < 10000" $ do
            compute 10_000 `shouldBe` "249"
