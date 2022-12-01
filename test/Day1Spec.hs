module Day1Spec (spec) where

import Test.Hspec
import Day1

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
    it "computes the maxCalories carried by one elf" $ do
      (maxCalories ex1) `shouldBe` 24000