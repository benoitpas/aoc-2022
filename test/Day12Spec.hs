module Day12Spec (spec) where

import Test.Hspec
import Day12

import qualified Data.Map as M

ex1 =  ["Sabqponm",
        "abcryxxl",
        "accszExk",
        "acctuvwj",
        "abdefghi"]

(tMap, startP, endP, _, _) = prepare ex1

dMap = shortestDistance tMap startP endP

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
        it "finds the shortest distance from start to all points" $ do
            dMap `shouldBe` M.fromList [
                ((0,0),(0,(0,0))),((0,1),(1,(0,0))),((0,2),(2,(0,1))),((0,3),(3,(0,2))),((0,4),(4,(0,3))),
                ((1,0),(1,(0,0))),((1,1),(2,(0,1))),((1,2),(3,(1,1))),((1,3),(4,(1,2))),((1,4),(5,(0,4))),
                ((2,0),(2,(1,0))),((2,1),(3,(1,1))),((2,2),(4,(1,2))),((2,3),(5,(1,3))),((2,4),(6,(2,3))),
                ((3,0),(19,(4,0))),((3,1),(20,(3,0))),((3,2),(21,(3,1))),((3,3),(22,(3,2))),((3,4),(7,(2,4))),
                ((4,0),(18,(5,0))),((4,1),(29,(5,1))),((4,2),(30,(4,1))),((4,3),(23,(3,3))),((4,4),(8,(3,4))),
                ((5,0),(17,(6,0))),((5,1),(28,(6,1))),((5,2),(31,(4,2))),((5,3),(24,(4,3))),((5,4),(9,(4,4))),
                ((6,0),(16,(7,0))),((6,1),(27,(6,2))),((6,2),(26,(6,3))),((6,3),(25,(5,3))),((6,4),(10,(5,4))),
                ((7,0),(15,(7,1))),((7,1),(14,(7,2))),((7,2),(13,(7,3))),((7,3),(12,(7,4))),((7,4),(11,(6,4)))]

    describe "findPath" $ do
        it "finds the shortest path from start to end" $ do
            (findPath dMap endP) `shouldBe` [(0,0),(0,1),(1,1),(1,2),(1,3),(2,3),(2,4),(3,4),(4,4),
                (5,4),(6,4),(7,4),(7,3),(7,2),(7,1),(7,0),(6,0),(5,0),(4,0),(3,0),(3,1),(3,2),(3,3),
                (4,3),(5,3),(6,3),(6,2),(6,1),(5,1),(4,1),(4,2),(5,2)]

    describe "findTrail" $ do
        it "finds the trail to go to the summit" $ do
            (length (findTrail tMap dMap endP) - 1) `shouldBe` 29