module Day10Spec (spec) where

import Test.Hspec
import Day10

import qualified Data.Map as M

ex1 =  ["noop",
        "addx 3",
        "addx -5"]

spec :: Spec
spec = do
    describe "runProgram" $ do
        it "runs example 1 program" $ do
            (runProgram ex1) `shouldBe` [(1,1),(2,1),(3,1),(4,4),(5,4)]
 
    describe "sumSignals" $ do
        it "compute the sum for example 2 program" $ do {
            content <-readFile "test/day10_example.txt";
            (sumSignals . runProgram . lines) content `shouldBe` 13140
        }
    describe "generateScreen" $ do
        it "generate the screen for example 2" $ do {
            content <-readFile "test/day10_example.txt";
--            (runProgram . lines) content `shouldBe` []
            (generateScreen . runProgram . lines) content `shouldBe` [
                "##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ",
                "###   ###   ###   ###   ###   ###   ### ",
                "####    ####    ####    ####    ####    ",
                "#####     #####     #####     #####     ",
                "######      ######      ######      ####",
                "#######       #######       #######     "]
        }