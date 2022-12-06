module Day6
    (
        findMarker,
        run,
    ) where

import qualified Data.List as L

findMarker s = let next (i,r) = (i+1, tail r) in
               let (ret,_) = until (\(_,r) -> let b = take 4 r in length b == length (L.nub b)) next (4, s)
               in ret

run :: IO ()
run = do
    content <- readFile "src/day6_input.txt"
    let l = (lines content)
    print ("puzzle 1: " ++ (show (findMarker (head l))))
