module Day8
    (
        toPoints,
        findVisibity,
        run,
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M

type Point = ((Int,Int), Char)

toPoints :: [String] -> [Point]
toPoints rows = 
    let addIndex l = [0..(length l - 1)] `zip` l in
    let wIndex = addIndex (map addIndex rows)
    in wIndex >>= \(y,row) -> row >>= (\(x,c) -> [((x,y),c)])

findVisibity :: [[Char]] -> [(Int, Int)]
findVisibity l =
    let nRows = length l in
    let yMax = nRows - 1 in
    let nCols = length (head l) in
    let xMax = nCols -1 in
    let terrain = M.fromList (toPoints l) in
    let mapTerrain p = terrain M.! p in
    let zeroMinus1 = C.chr (C.ord '0'  - 1) in
    let visible points = let (r,_) = foldl (\(vl,pc) p -> let c = mapTerrain p in if pc < c then (p:vl, c) else (vl,pc)) ([], zeroMinus1) points 
                         in r in
    let hVisible xRange = [0..yMax] >>= (\y -> let h = map (\x -> (x,y)) xRange in visible h) in
    let h1 =  hVisible [0..xMax] in
    let h2 =  hVisible [xMax, xMax-1..0] in
    let vVisible yRange = [0..xMax] >>= (\x -> let v = map (\y -> (x,y)) yRange in visible v) in
    let v1 =  vVisible [0..yMax] in
    let v2 =  vVisible [yMax, yMax-1..0]
    in L.sort $ h1 `L.union` h2 `L.union` v1 `L.union` v2

run :: IO ()
run = do
    content <- readFile "src/day8_input.txt"
    let l = (lines content)
    print ("puzzle 1: " ++ (show (length (findVisibity l))))
