module Day2Spec (spec) where

import Test.Hspec
import Day2

ex1 = [ "A Y",
        "B X",
        "C Z"]

spec :: Spec
spec = do
  describe "totalScore" $ do
    it "computes the max Calories carried by one elf" $ do
        (totalScore ex1) `shouldBe` 15