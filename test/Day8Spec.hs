module Day8Spec (spec) where

import Test.Hspec
import Day8

import qualified Data.Map as M

ex1 =  ["30373",
        "25512",
        "65332",
        "33549",
        "35390"]

spec :: Spec
spec = do
  describe "toPoints" $ do
    it "convert the 'map' to points" $ do
      (toPoints ex1) `shouldBe` [((0,0),'3'),((1,0),'0'),((2,0),'3'),((3,0),'7'),((4,0),'3'),
                                 ((0,1),'2'),((1,1),'5'),((2,1),'5'),((3,1),'1'),((4,1),'2'),
                                 ((0,2),'6'),((1,2),'5'),((2,2),'3'),((3,2),'3'),((4,2),'2'),
                                 ((0,3),'3'),((1,3),'3'),((2,3),'5'),((3,3),'4'),((4,3),'9'),
                                 ((0,4),'3'),((1,4),'5'),((2,4),'3'),((3,4),'9'),((4,4),'0')]

  describe "findVisibity" $ do
    it "find which points are visible" $ do
      (findVisibity ex1) `shouldBe` [(0,0),(0,1),(0,2),(0,3),(0,4),
                                     (1,0),(1,1),(1,2),      (1,4),
                                     (2,0),(2,1),      (2,3),(2,4),
                                     (3,0),      (3,2),      (3,4),
                                     (4,0),(4,1),(4,2),(4,3),(4,4)]