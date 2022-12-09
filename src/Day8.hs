module Day8
    (
        toPoints,
        findVisibity,
        run,
    ) where

import qualified Data.List as L
import qualified Data.Map as M

type Point = ((Int,Int), Char)

toPoints :: [String] -> [Point]
toPoints rows = 
    let addIndex l = [0..(length l - 1)] `zip` l in
    let wIndex = addIndex (map addIndex rows)
    in wIndex >>= \(y,row) -> row >>= (\(x,c) -> [((x,y),c)])

findVisibity l =
    let nRows = length l in
    let yMax = nRows - 1 in
    let nCols = length (head l) in
    let xMax = nCols -1 in
    let points = M.fromList (toPoints l) in
    let visibily = M.fromList (     ([0..xMax] >>= (\x -> [((x,0),True),((x,yMax),True)])) 
                                ++  ([1..yMax-1] >>= (\y -> [((0,y),True),((xMax,y),True)]))) in
    let visible p m = Nothing in
    let notVisible p m = Nothing in
    let next vMap = let points = [0..xMax]  >>=  (\x -> ([0..yMax] >>= (\y -> [(x,y)]))) 
                    in foldr (\p m -> case (M.member p m, visible p m, notVisible p m) of
                                        (False, Just True, _) -> M.insert p True m
                                        (False, _, Just True) -> M.insert p False m
                                        _                     -> m) vMap points
    in next visibily

--    let getVisibity (x,y) vMap = let side = (x == 0) || (y == 0) || (x == (nCols - 1)) || (y == (nRows - 1)) 
--                                 in if side || M.member (x,y) vMap then (side || vMap M.! (x,y), vMap)
--                                                                   else let c = points M.! (x,y) in 
--                                                                        (let (v1, vMap1) = getVisibity (x-1,y) vMap 
--                                                                        in (v1 && points M.! (x-1,y) < c || let (v2, vMap2) = getVisibity (x,y-1) vMap1 in (v2 && points M.! (x,y-1) < c) || 
--                                                                        let (v3, vMap3) = getVisibity (x+1,y) vMap2 in (v3 && points M.! (x+1,y) < c) || 
--                                                                        let (v4, vMap4) = getVisibity (x,y+1) vMap3
--                                                                        in (v4 && points M.! (x,y+1) < c)), vMap4)
--    in getVisibity (nCols `div` 2, nRows `div` 2) visibily

run :: IO ()
run = do
    content <- readFile "src/day8_input.txt"
    let l = (lines content)
    let points = toPoints l
    print ("puzzle 1: " ++ (show (M.fromList points)))
