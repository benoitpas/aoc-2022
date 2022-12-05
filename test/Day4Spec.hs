module Day4Spec (spec) where

import Test.Hspec
import Day4

ex1 :: [String]
ex1 =  ["2-4,6-8",
        "2-3,4-5",
        "5-7,7-9",
        "2-8,3-7",
        "6-6,4-6",
        "2-6,4-8"]

spec :: Spec
spec = do
  describe "overlap" $ do
    it "find if the intervals overlap" $ do
        (map contains ex1) `shouldBe` [False,False,False,True,True,False]
