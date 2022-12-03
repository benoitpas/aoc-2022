module Day3Spec (spec) where

import Test.Hspec
import Day3

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
