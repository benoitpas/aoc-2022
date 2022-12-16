module Day9Spec (spec) where

import Test.Hspec
import Day9

import qualified Data.Set as S

ex1 =  ["R 4",
        "U 4",
        "L 3",
        "D 1",
        "R 4",
        "D 1",
        "L 5",
        "R 2"]

ex2 = [ "R 5",
        "U 8",
        "L 8",
        "D 3",
        "R 17",
        "D 10",
        "L 25",
        "U 20"]

spec :: Spec
spec = do
    describe "movePoint" $ do
        it "does not move the point in the tail 1" $ do
            (movePoint (0,0) (1,0)) `shouldBe` (0,0)
        it "does not move the point in the tail 2" $ do
            (movePoint (1,0) (0,1)) `shouldBe` (1,0)
        it "does not move the point in the tail 3" $ do
            (movePoint (0,0) (0,0)) `shouldBe` (0,0)
        it "moves the point in tail 1" $ do
            (movePoint (0,0) (2,0)) `shouldBe` (1,0)
        it "moves the point in the tail 2" $ do
            (movePoint (0,0) (0,2)) `shouldBe` (0,1)
        it "moves the point in the tail 3" $ do
            (movePoint (0,0) (1,2)) `shouldBe` (1,1)
        it "moves the point in the tail 4" $ do
            (movePoint (0,0) (2,1)) `shouldBe` (1,1)

    describe "moveTail" $ do
        it "doesn't move the tail 1" $ do
            (moveTail (0,0) [(0,0),(0,0)]) `shouldBe` [(0,0),(0,0)]
        it "doesn't move the tail 2" $ do
            (moveTail (1,0) [(0,0),(0,0)]) `shouldBe` [(0,0),(0,0)]
        it "moves the tail 1" $ do
            (moveTail (2,0) [(0,0),(0,0)]) `shouldBe` [(1,0),(0,0)]
        it "moves the tail 2" $ do
            (moveTail (3,0) [(1,0),(0,0)]) `shouldBe` [(2,0),(1,0)]
        it "moves the tail 3" $ do
            (moveTail (3,1) [(2,0),(1,0)]) `shouldBe` [(2,0),(1,0)]
        it "moves the tail 4" $ do
            (moveTail (3,2) [(2,0),(1,0)]) `shouldBe` [(3,1),(2,1)]

    describe "addpoints" $ do
        it "finds more visited points" $ do
            (addpoints [(1,0),(2,0),(3,0),(4,0)] [(0,0),(0,0),(0,0)] S.empty) `shouldBe` ((4,0),[(3,0),(2,0),(1,0)], S.fromList [(0,0),(1,0)])
            (addpoints [(4,1),(4,2),(4,3),(4,4)] [(3,0),(2,0),(1,0)] (S.fromList [(0,0),(1,0)])) `shouldBe` ((4,4),[(4,3),(4,2),(3,2)], S.fromList [(0,0),(1,0),(2,1),(3,2)])


    describe "tailPositions" $ do
        it "find the positions where the tail went in example 1" $ do
            (tailPositions ex1 1) `shouldBe` S.fromList [               (2,-4),(3,-4),
                                                                               (3,-3),(4,-3),
                                                                 (1,-2),(2,-2),(3,-2),(4,-2),
                                                                                      (4,-1),
                                                          (0, 0),(1, 0),(2, 0),(3, 0)]

        it "find the number of positions where the tail went in example 2" $ do
            (length $ tailPositions ex2 9) `shouldBe` 36