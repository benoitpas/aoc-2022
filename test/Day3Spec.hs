module Day3Spec (spec) where

import Test.Hspec
import Day3

ex1 :: [String]
ex1 =  ["vJrwpWtwJgWrhcsFMMfFFhFp",
        "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
        "PmmdzqPrVvPwwTWBwg",
        "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
        "ttgJtRGJQctTZtZT",
        "CrZsJsPPZsGzwwsLwLmpwMDw"]

spec :: Spec
spec = do
  describe "rucksackPriority" $ do
    it "computes the total priority of the item in the rucksack" $ do
        (rucksackPriority (head ex1)) `shouldBe` 16

  describe "totalPriority" $ do
    it "computes the total priority of the items in all the rucksack" $ do
        (totalPriority ex1) `shouldBe` 157

  describe "sumBadges" $ do
    it "group sacks which have no elements in common, case with a mix " $ do
        (sumBadges ex1) `shouldBe` 70
