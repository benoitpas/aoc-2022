module Day1Spec (spec) where

import Test.Hspec
import Day1

ex1 :: [String]
ex1 = ["1000",
       "2000",
       "3000",
        "",
       "4000",
       "",
       "5000",
       "6000",
       "",
       "7000",
       "8000",
       "9000",
       "",
       "10000"]

spec :: Spec
spec = do
  describe "maxCalories" $ do
    it "computes the max Calories carried by one elf" $ do
      maximum (calories ex1) `shouldBe` 24000

  describe "max3Calories" $ do
    it "computes the max Calories carried by three elves" $ do
      max3Calories (calories ex1) `shouldBe` 45000