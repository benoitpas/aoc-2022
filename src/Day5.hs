module Day5
    (
        parseInput,
        getMessage,
        run,
    ) where

import qualified Data.List as L
import qualified Data.Map as M

parseInput :: [String] -> (M.Map Int [Char], [(Int,Int,Int)])
parseInput l = 
    let (stackStrings,rest) = break (L.elem '1') l in
    let indices = map read (words (head rest)) in
    let stacks = M.fromList $ map (\i -> (i,[0..(length stackStrings -1)] >>= (\j -> let c = (stackStrings !! j) !! (-3+4*i) 
                                                                                     in if c == ' ' then [] else [c]))) indices in
    let moves = map (\s -> let w = L.words s in (read (w !! 1), read (w !! 3), read (w !! 5))) (L.drop 2 rest)
    in (stacks, moves)

getMessage :: (M.Map Int [Char], [(Int,Int,Int)]) -> [Char]
getMessage (stacks, moves) = 
    let finalStacks = foldl (\s (n,si,di) -> let source = s M.! si in
                                             let s2 = (M.insert si (drop n source) s)
                                             in (M.insert di (reverse (take n source) ++ (s M.! di)) s2)) stacks moves
    in (M.elems finalStacks) >>= (take 1) 

run :: IO ()
run = do
    content <- readFile "src/day5_input.txt"
    let l = (lines content)
    let pInput = parseInput l
    print ("puzzle 1: " ++ (show (getMessage pInput)))
