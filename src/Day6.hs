module Day6
    (
        findMarker,
        run,
    ) where

import qualified Data.List as L

findMarker :: Eq a => Int -> [a] -> Int
findMarker n s = let next (i,r) = (i+1, tail r) in
                 let (ret,_) = until (\(_,r) -> let b = take n r in length b == length (L.nub b)) next (n, s)
                 in ret

run :: IO ()
run = do
    content <- readFile "src/day6_input.txt"
    let l = (lines content)
    print ("puzzle 1: " ++ (show (findMarker 4 (head l))))
    print ("puzzle 2: " ++ (show (findMarker 14 (head l))))
