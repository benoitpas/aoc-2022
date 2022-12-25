module Day12
    (
        toPoints,
        shortestDistance,
        run,
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

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
            foldr (\(d,p,sp) td -> case L.find (\(d1,p1,sp1) -> p1 == p) td of
                                    Just (d1,_,sp1) -> if d<d1 then L.insert (d,p,sp) td else td
                                    _ -> L.insert (d,p,sp) td) td1 td2
    in case todo of
            (d,(x,y),sp):r -> let pc = tmap M.! (x,y) in
                              let distance npc = 1 + abs (C.ord npc - C.ord pc) in
                              let nextPoints = directions >>=(\(dx,dy) -> let np = (x+dx,y+dy)
                                                                          in case (M.lookup np tmap, M.member np done) of
                                                                                (Just npc, False) -> let dd = distance npc
                                                                                                     in if dd <=2 then [(d + 1,np,(x,y))]
                                                                                                                  else []
                                                                                _ -> [])
                              in (mergeTodo r nextPoints, M.insert (x,y) (d,sp) done)
            _ -> (todo, done)

shortestDistance l =
    let tPoints = toPoints l in
    let findPoint cToFind = head $ tPoints >>= (\(p,c) -> if c == cToFind then [p] else []) in
    let startP = findPoint 'S' 
        endP = findPoint 'E' in
    let tMap = M.insert startP 'a' (M.insert endP 'z' (M.fromList tPoints)) in
    let next' = next tMap in
    let (_,done) = until (\(todo,_) -> length todo == 0) next' ([(0,startP,startP)], M.empty) in
    let Just (r,_) = M.lookup endP done
    in r

run :: IO ()
run = do
    content <- readFile "src/day12_input.txt"
    let l = (lines content)

    putStrLn ("puzzle 1: " ++ (show (shortestDistance l))) -- 5904 points = 41*144
    