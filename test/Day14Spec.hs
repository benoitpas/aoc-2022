module Day14Spec (spec) where

import Test.Hspec
import Day14

import qualified Data.Map as M

ex1 = [ "498,4 -> 498,6 -> 496,6",
        "503,4 -> 502,4 -> 502,9 -> 494,9"]
spec :: Spec 
spec = do
    describe "parse" $ do
        it "parses the lines" $ do
            (parseLines  ex1) `shouldBe` [[(498,4), (498,6), (496,6)],
                                          [(503,4), (502,4), (502,9), (494,9)]]

    describe "addRocks" $ do
        it "add the rocks corresponding to a line to the terrain" $ do
            (addRocks M.empty [(498,4), (498,6), (496,6)]) `shouldBe` (M.fromList [((496,6),'#'),((497,6),'#'),((498,4),'#'),((498,5),'#'),((498,6),'#')])
