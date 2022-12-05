module Day4Spec (spec) where

import Test.Hspec
import Day4

ex1 :: [String]
ex1 =  ["2-4,6-8",
        "6-8,2-4",
        "2-3,4-5",
        "4-5,2-3",
        "5-7,7-9",
        "7-9,5-7",
        "2-8,3-7",
        "3-7,2-8",
        "6-6,4-6",
        "4-6,6-6",
        "2-6,4-8",
        "4-8,2-6"]

spec :: Spec
spec = do
  describe "contains" $ do
    it "find if the intervals contains one or the other" $ do
        (map contains ex1) `shouldBe` [False,False,False,False,False,False,True,True,True,True,False,False]

  describe "overlap" $ do
    it "find if the intervals overlap" $ do
        (map overlap ex1) `shouldBe` [False,False,False,False,True,True,True,True,True,True,True,True]