module Day2Spec (spec) where

import Test.Hspec
import Day2

ex1 = [ "A Y",
        "B X",
        "C Z"]

spec :: Spec
spec = do
  describe "score1" $ do
    it "computes the score with interpretation 1" $ do
        (totalScore score1 ex1) `shouldBe` 15
    it "computes the score with interpretation 2" $ do
        (totalScore score2 ex1) `shouldBe` 12