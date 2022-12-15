module Day9
    (
        moveTail,
        tailPositions,
        run,
    ) where

import qualified Data.Set as S

moveTail (tx,ty) (hx,hy) = if abs (hx-tx) > 1 || abs (hy-ty) > 1 
                            then (tx + signum (hx-tx), ty + signum (hy-ty))
                            else (tx, ty)

tailPositions cmds = 
    let addpoints hPoints (tx,ty) prev = foldl (\((hx,hy),(tx,ty),prev) (nhx,nhy) -> let (ntx,nty) = moveTail (tx,ty) (nhx,nhy)
                                                                                     in ((nhx,nhy), (ntx,nty), S.insert (ntx,nty) prev)) (head hPoints, (tx,ty), prev) (tail hPoints) in
    let next ((hx,hy), pTail, prev) cmd = case words cmd of
                                            ["R",n] -> addpoints (map (\x -> (x,hy)) [hx..hx+read n]) pTail prev
                                            ["L",n] -> addpoints (map (\x -> (x,hy)) [hx, hx-1..hx-read n]) pTail prev
                                            ["D",n] -> addpoints (map (\y -> (hx,y)) [hy..hy+read n]) pTail prev
                                            ["U",n] -> addpoints (map (\y -> (hx,y)) [hy, hy-1..hy-read n]) pTail prev in
    let (_,_,r) = foldl next ((0,0), (0,0), S.empty) cmds
    in r


run :: IO ()
run = do
    content <- readFile "src/day9_input.txt"
    let l = (lines content)
    print ("puzzle 1: " ++ (show ( length (tailPositions l))))
