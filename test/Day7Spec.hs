module Day7Spec (spec) where

import Test.Hspec
import Day7

ex1 =  ["$ cd /",
        "$ ls",
        "dir a",
        "14848514 b.txt",
        "8504156 c.dat",
        "dir d",
        "$ cd a",
        "$ ls",
        "dir e",
        "29116 f",
        "2557 g",
        "62596 h.lst",
        "$ cd e",
        "$ ls",
        "584 i",
        "$ cd ..",
        "$ cd ..",
        "$ cd d",
        "$ ls",
        "4060174 j",
        "8033020 d.log",
        "5626152 d.ext",
        "7214296 k"]


spec :: Spec
spec = do
  describe "parse" $ do
    it "parse the example tree" $ do
      (parse ex1) `shouldBe` (Directory "." 48381165 [File "b.txt" 14848514, File "c.dat" 8504156]
                                              [Directory "d" 24933642 [File "j" 4060174, File "d.log" 8033020, File "d.ext" 5626152, File "k" 7214296] [],
                                               Directory "a" 94853 [File "f" 29116, File "g" 2557, File "h.lst" 62596] [Directory "e" 584 [File "i" 584] []]])
  describe "puzzle1" $ do
    it "computes the sum for puzzle 1" $ do
      (puzzle1 (parse ex1)) `shouldBe` 95437

  describe "puzzle2" $ do
    it "finds the the file size for puzzle 1" $ do
      (puzzle2 (parse ex1)) `shouldBe` 24933642