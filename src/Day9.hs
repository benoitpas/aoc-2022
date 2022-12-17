module Day9
    (
        movePoint,
        moveTail,
        addpoints,
        tailPositions,
        run,
    ) where

import qualified Data.Set as S

movePoint (tx,ty) (hx,hy) = if abs (hx-tx) > 1 || abs (hy-ty) > 1
                            then (tx + signum (hx-tx), ty + signum (hy-ty))
                            else (tx, ty)

moveTail hPoint tailPoints  = tail $ foldl (\tp np -> let nnp = movePoint np (last tp) in tp ++ [nnp]) [hPoint] tailPoints

addpoints hPoints tailPoints prevPoints = 
    foldl (\(_,tPoints,prev) (nhx,nhy) -> let newTail = moveTail (nhx,nhy) tPoints 
                                          in ((nhx,nhy), newTail, S.insert (last newTail) prev)
          ) (head hPoints, tailPoints, prevPoints) hPoints

tailPositions cmds nTail =
    let next ((hx,hy), tailPoints, prev) cmd = case words cmd of
                                                    ["R",n] -> addpoints [(x,hy) | x <- [hx+1..hx+read n]]       tailPoints prev
                                                    ["L",n] -> addpoints [(x,hy) | x <- [hx-1, hx-2..hx-read n]] tailPoints prev
                                                    ["D",n] -> addpoints [(hx,y) | y <- [hy+1..hy+read n]]       tailPoints prev
                                                    ["U",n] -> addpoints [(hx,y) | y <- [hy-1, hy-2..hy-read n]] tailPoints prev in
    let (_,_,r) = foldl next ((0,0), replicate nTail (0,0), S.empty) cmds
    in r

run :: IO ()
run = do
    content <- readFile "src/day9_input.txt"
    let l = (lines content)
    print ("puzzle 1: " ++ (show (length (tailPositions l 1))))
    print ("puzzle 2: " ++ (show (length (tailPositions l 9))))
