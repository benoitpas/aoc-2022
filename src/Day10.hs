module Day10
    (
        sumSignals,
        runProgram,
        generateScreen,
        run,
    ) where

import qualified Data.List as L
import qualified Data.Map as M

runProgram :: [String] -> [(Int,Int)]
runProgram cmds =
    let (_,_,r) = foldl (\(x,c,m) cmd -> case words cmd of
                                            ["noop"]    -> (x, c+1, M.insert c x m)
                                            ["addx", n] -> let nx = x + read n 
                                                           in (nx, c+2, M.union (M.fromList[(c,x),(c+1,x)]) m)
                        ) (1,1,M.fromList []) cmds
    in M.toList r

sumSignals :: [(Int,Int)] -> Int
sumSignals l = 
    let cycles = \(p,_) -> L.elem p [20,60..220]
    in sum $ map (\(c,x) -> c*x) (filter cycles l)

generateScreen ::  [(Int,Int)] -> String
generateScreen l = let chars = map (\(c,x) -> let cm = (c - 1) `mod` 40 + 1 in if x <= cm && cm <= x+2 then '#' else ' ') l in
                   let (r,_) = until (\(_,c) -> c == []) (\(l2,c) -> let (h,nc) = L.splitAt 40 c in (l2 ++ [h], nc)) ([],chars)
                   in unlines r

run :: IO ()
run = do 
    content <- readFile "src/day10_input.txt"
    let l = runProgram (lines content)
    putStrLn ("puzzle 1: " ++ (show (sumSignals l)))
    putStrLn "puzzle 2: " 
    putStrLn (generateScreen l)