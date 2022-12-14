module Day8
    (
        toPoints,
        findVisibity,
        scenicScore,
        maxScenicScore,
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

prepareTerrain :: [[Char]] -> ((Int, Int) -> Char, (Int, Int))
prepareTerrain l = let nRows = length l in
    let yMax = nRows - 1 in
    let nCols = length (head l) in
    let xMax = nCols -1 in
    let terrain = M.fromList (toPoints l) in
    let mapTerrain p = terrain M.! p
    in (mapTerrain, (xMax, yMax))

findVisibity :: [[Char]] -> [(Int, Int)]
findVisibity = (uncurry iFindVisibity) . prepareTerrain

iFindVisibity :: ((Int, Int) -> Char) -> (Int, Int) -> [(Int, Int)]
iFindVisibity mapTerrain (xMax,yMax) =
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

scenicScore l p = let (mapTerrain, pMax) = prepareTerrain l
                  in iScenicScore mapTerrain pMax p

iScenicScore mapTerrain (xMax,yMax) (x,y) =
    let refc = mapTerrain (x,y) in
    let count (before,after) = length before + if length after > 0 then 1 else 0 in
    let scoreH xRange = count $ span (\p -> mapTerrain p < refc) (map (\x -> (x,y)) xRange) in
    let scoreV yRange = count $ span (\p -> mapTerrain p < refc) (map (\y -> (x,y)) yRange)
    in scoreH [x-1,x-2..0] * scoreH [x+1..xMax] * scoreV [y-1,y-2..0] * scoreV [y+1..yMax]

maxScenicScore l =
    let (mapTerrain, (xMax,yMax)) = prepareTerrain l in
    let points = do { x <- [1..xMax-1] ; y <- [1..yMax -1] ; [(x,y)]}
    in L.maximum $ map (iScenicScore mapTerrain (xMax,yMax)) points

run :: IO ()
run = do
    content <- readFile "src/day8_input.txt"
    let l = (lines content)
    print ("puzzle 1: " ++ (show (length (findVisibity l))))
    print ("puzzle 2: " ++ (show (maxScenicScore l)))    