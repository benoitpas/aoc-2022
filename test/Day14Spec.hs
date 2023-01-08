module Day14Spec (spec) where

import Test.Hspec
import Day14

ex1 = [ "498,4 -> 498,6 -> 496,6",
        "503,4 -> 502,4 -> 502,9 -> 494,9"]
spec :: Spec 
spec = do
    describe "parse" $ do
        it "parses the lines" $ do
            (parseLines  ex1) `shouldBe` [[(498,4), (498,6), (496,6)],
                                          [(503,4), (502,4), (502,9), (494,9)]]
