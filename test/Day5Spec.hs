module Day5Spec (spec) where

import Test.Hspec
import Day5

import qualified Data.Map as M

ex1 :: [String]
ex1 =  ["    [D]    ",
        "[N] [C]    ",
        "[Z] [M] [P]",
        " 1   2   3 ",
        "",
        "move 1 from 2 to 1",
        "move 3 from 1 to 3",
        "move 2 from 2 to 1",
        "move 1 from 1 to 2"]

spec :: Spec
spec = do
  describe "parseInput" $ do
    it "parse the puzzle input" $ do
        (parseInput ex1) `shouldBe` (M.fromList [(1,"NZ"),(2,"DCM"),(3,"P")],
                                     [(1,2,1),(3,1,3),(2,2,1),(1,1,2)])

  describe "getMessage" $ do
    it "get Message from input" $ do
        (getMessage (parseInput ex1)) `shouldBe` "CMZ"