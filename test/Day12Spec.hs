module Day12Spec (spec) where

import Test.Hspec
import Day12

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


ex1 =  ["Sabqponm",
        "abcryxxl",
        "accszExk",
        "acctuvwj",
        "abdefghi"]
    

spec :: Spec 
spec = do
    describe "toPoints" $ do
        it "generates the points from a grid" $ do
            (toPoints  ex1) `shouldBe` [((0,0),'S'),((1,0),'a'),((2,0),'b'),((3,0),'q'),((4,0),'p'),((5,0),'o'),((6,0),'n'),((7,0),'m'),
                                        ((0,1),'a'),((1,1),'b'),((2,1),'c'),((3,1),'r'),((4,1),'y'),((5,1),'x'),((6,1),'x'),((7,1),'l'),
                                        ((0,2),'a'),((1,2),'c'),((2,2),'c'),((3,2),'s'),((4,2),'z'),((5,2),'E'),((6,2),'x'),((7,2),'k'),
                                        ((0,3),'a'),((1,3),'c'),((2,3),'c'),((3,3),'t'),((4,3),'u'),((5,3),'v'),((6,3),'w'),((7,3),'j'),
                                        ((0,4),'a'),((1,4),'b'),((2,4),'d'),((3,4),'e'),((4,4),'f'),((5,4),'g'),((6,4),'h'),((7,4),'i')]
    describe "shortestDistance" $ do
        it "finds the shortest distance from start to end" $ do
            (shortestDistance ex1) `shouldBe` 31