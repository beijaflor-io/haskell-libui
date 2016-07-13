module SanitySpec where

import Test.Hspec

spec = describe "when I have tests" $
    it "I have sanity" $ True `shouldBe` True
