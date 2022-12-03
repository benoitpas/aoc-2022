module Day3
    (
        priority,
        rucksackPriority,
        totalPriority,
        run,
    ) where

import qualified Data.Char as C
import qualified Data.Set as S

priority c
    | 'a' <= c && c <= 'z' = C.ord c - C.ord 'a' + 1
    | 'A' <= c && c <= 'Z' = C.ord c - C.ord 'A' + 27

rucksackPriority l = 
    let (rs1,rs2) = splitAt ((length l) `div` 2) l in
    let e =  S.elemAt 0 ((S.fromList rs1) `S.intersection` (S.fromList rs2)) -- no error checking, would fail on incorrect data
    in priority e

totalPriority allLines = sum (map rucksackPriority allLines)

run :: IO ()
run = do
    content <- readFile "src/day3_input.txt"
    let l = (lines content)
    print ("puzzle 1: " ++ (show (totalPriority l)))
