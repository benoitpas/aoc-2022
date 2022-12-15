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

spec :: Spec
spec = do
    describe "moveTail" $ do
        it "does not move the tail 1" $ do
            (moveTail (0,0) (1,0)) `shouldBe` (0,0)
        it "does not move the tail 2" $ do
            (moveTail (1,0) (0,1)) `shouldBe` (1,0)
        it "does not move the tail 3" $ do
            (moveTail (0,0) (0,0)) `shouldBe` (0,0)
        it "moves the tail 1" $ do
            (moveTail (0,0) (2,0)) `shouldBe` (1,0)
        it "moves the tail 2" $ do
            (moveTail (0,0) (0,2)) `shouldBe` (0,1)
        it "moves the tail 3" $ do
            (moveTail (0,0) (1,2)) `shouldBe` (1,1)
        it "moves the tail 4" $ do
            (moveTail (0,0) (2,1)) `shouldBe` (1,1)

    describe "tailPositions" $ do
        it "find the number of positions where the tail went" $ do
            (tailPositions ex1) `shouldBe` S.fromList [               (2,-4),(3,-4),
                                                                             (3,-3),(4,-3),
                                                               (1,-2),(2,-2),(3,-2),(4,-2),
                                                                                    (4,-1),
                                                        (0, 0),(1, 0),(2, 0),(3, 0)]