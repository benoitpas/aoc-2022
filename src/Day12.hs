module Day12
    (
        toPoints,
        prepare,
        shortestDistance,
        findPath,
        findTrail,
        run,
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M

import Codec.Picture
import Data.Word

type Point = (Int,Int)

toPoints :: [String] -> [(Point,Char)]
toPoints rows = 
    let addIndex l = [0..(length l - 1)] `zip` l in
    let wIndex = addIndex (map addIndex rows)
    in wIndex >>= \(y,row) -> row >>= (\(x,c) -> [((x,y),c)])


-- [(Int,Point,Point)] contains an ordered list which first points is the current point and the second one the previous points
next :: M.Map Point Char -> ([(Int,Point,Point)], M.Map Point (Int,Point)) -> ([(Int,Point,Point)], M.Map Point (Int,Point)) 
next tmap (todo, done) = 
    let directions = [(1,0),(0,1),(-1,0),(0,-1)] in
    let mergeTodo td1 td2 =
            foldr (\(d,p,sp) td -> case L.find (\(_,p1,_) -> p1 == p) td of
                                    Just (d1,_,_) -> if d<d1 then L.insert (d,p,sp) td else td
                                    _ -> L.insert (d,p,sp) td) td1 td2
    in case todo of
            (d,(x,y),sp):r -> let pc = tmap M.! (x,y) in
                              let canMove npc = npc <= pc || (C.ord npc - C.ord pc) <= 1 in
                              let nextPoints = directions >>=(\(dx,dy) -> let np = (x+dx,y+dy)
                                                                          in case (M.lookup np tmap, M.member np done) of
                                                                                (Just npc, False) -> if canMove npc then [(d + 1,np,(x,y))]
                                                                                                                    else []
                                                                                _ -> [])
                              in (mergeTodo r nextPoints, M.insert (x,y) (d,sp) done)
            _ -> (todo, done)

prepare l =
    let nbCols = (length (head l))
        nbRows = (length l) in
    let tPoints = toPoints l in
    let findPoint cToFind = head $ [p | (p,c) <- tPoints, c == cToFind ] in
    let startP = findPoint 'S' 
        endP = findPoint 'E' in
    let tMap = M.insert startP 'a' (M.insert endP 'z' (M.fromList tPoints))
    in (tMap, startP, endP, nbCols, nbRows)

shortestDistance tMap startP endP =
    let next' = next tMap in
    let (_,done) = until (\(todo,_) -> length todo == 0) next' ([(0,startP,startP)], M.empty) in
    done

findPath tMap endP =
    let next (path,_) = let p = head path in
                        let (_,np) = tMap M.! p in
                        let finished = p == np
                        in (if finished then path else np:path, finished) in
    let (r,_) = until snd next ([endP], False)
    in r

colors :: Char -> (Word8,Word8,Word8)
colors 'a' = (0x10,0x10,0x00)
colors 'b' = (0x00,0x10,0x10)
colors 'c' = (0x10,0x00,0x10)
colors 'd' = (0x20,0x20,0x00)
colors 'e' = (0x00,0x20,0x20)
colors 'f' = (0x20,0x00,0x20)
colors 'g' = (0x30,0x30,0x00)
colors 'h' = (0x00,0x30,0x30)
colors 'i' = (0x30,0x00,0x30)
colors 'j' = (0x40,0x40,0x00)
colors 'k' = (0x00,0x40,0x40)
colors 'l' = (0x40,0x00,0x40)
colors 'm' = (0x50,0x00,0x50)
colors 'n' = (0x00,0x50,0x50)
colors 'o' = (0x50,0x00,0x50)
colors 'p' = (0x60,0x60,0x00)
colors 'q' = (0x00,0x60,0x60)
colors 'r' = (0x60,0x00,0x60)
colors 's' = (0x70,0x70,0x00)
colors 't' = (0x00,0x70,0x70)
colors 'u' = (0x70,0x00,0x70)
colors 'v' = (0x80,0x80,0x00)
colors 'w' = (0x00,0x80,0x80)
colors 'x' = (0x80,0x00,0x80)
colors 'y' = (0x90,0x90,0x00)
colors 'z' = (0x00,0x90,0x90)

writeTerrain tMap path nbCols nbRows =
    let generatePixel x y = let (r,g,b) = case (L.elem (x,y) path, (tMap M.! (x,y))) of
                                                    (True,_) -> (0xEF,0xEF,0xEF) 
                                                    (_, c) -> colors c in PixelRGB8 r g b in
    let image = generateImage generatePixel nbCols nbRows
    in writeBitmap "terrain.bmp" image

writeHeatMap dMap nbCols nbRows =
    let e = map fst (M.elems dMap) in
    let dMax = maximum e in
    let generatePixel x y = let (r,g,b) = case M.lookup (x,y) dMap of
                                            Just (d,_) -> (toEnum (d * 250 `div` dMax), 0,0)
                                            _ -> (0x80,0x80,0x80) in PixelRGB8 r g b in
    let image = generateImage generatePixel nbCols nbRows
    in writeBitmap "heatmap.bmp" image

findTrail tMap dMap endP =
    let startingPoints = [p | (p,c) <- M.toList tMap, c == 'a'] in
    let trails = [findPath dMap2 endP | sp <- startingPoints ,let dMap2 = shortestDistance tMap sp endP, M.member endP dMap2]
    in head $ L.sortOn length trails

run :: IO ()
run = do
    content <- readFile "src/day12_input.txt"
    let l = (lines content)
    let (tMap, startP, endP, nbCols, nbRows) = prepare l
    let sm = shortestDistance tMap startP endP
    let path = findPath sm endP
    writeTerrain tMap path nbCols nbRows
    writeHeatMap sm nbCols nbRows
    let Just (p1,_) = M.lookup endP sm
    putStrLn ("puzzle 1: " ++ (show p1))
    let trail = findTrail tMap sm endP
    let p2 = length trail - 1
    putStrLn ("puzzle 2: " ++ (show p2))

    