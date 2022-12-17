module Day10
    (
        sumSignals,
        runProgram,
        run,
    ) where

import qualified Data.List as L
import qualified Data.Map as M

runProgram cmds = 
    let (_,_,r) = foldl (\(x,c,m) cmd -> case words cmd of
                                            ["noop"]    -> (x, c+1, M.insert c x m)
                                            ["addx", n] -> let nx = x + read n 
                                                           in (nx, c+2, M.union (M.fromList[(c,x),(c+1,x)]) m)
                        ) (1,1,M.fromList []) cmds
    in r

sumSignals l = 
    let cycles = \p _ -> L.elem p [20,60..220]
    in sum $ map (\(c,x) -> c*x) (M.toList (M.filterWithKey cycles l))

run :: IO ()
run = do 
    content <- readFile "src/day10_input.txt"
    let l = (lines content)
    print ("puzzle 1: " ++ (show (sumSignals( runProgram l))))