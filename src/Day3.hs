module Day3
    (
        rucksackPriority,
        totalPriority,
        sumBadges,
        run,
    ) where

import qualified Data.Char as C
import qualified Data.Set as S
import qualified Data.List as L

priority :: Char -> Int
priority c
    | 'a' <= c && c <= 'z' = C.ord c - C.ord 'a' + 1
    | 'A' <= c && c <= 'Z' = C.ord c - C.ord 'A' + 27
    |  True = 0

rucksackPriority :: [Char] -> Int
rucksackPriority l =
    let (rs1,rs2) = splitAt ((length l) `div` 2) l in
    let e =  S.elemAt 0 ((S.fromList rs1) `S.intersection` (S.fromList rs2)) -- no error checking, would fail on incorrect data
    in priority e

totalPriority :: [[Char]] -> Int
totalPriority allLines = sum (map rucksackPriority allLines)

sumBadges :: [[Char]] -> Int
sumBadges l =
    let next (acc, rlines) = let sacksPriority l1 l2 l3 = (priority . head) (l1 `L.intersect` l2 `L.intersect` l3)
                             in case rlines of
                                    l1:l2:l3:nlines -> (acc + sacksPriority l1 l2 l3, nlines)
                                    _ -> (acc,[]) in
    let (r,_) = until (null . snd) next (0,l)
    in r

run :: IO ()
run = do
    content <- readFile "src/day3_input.txt"
    let l = (lines content)
    print ("puzzle 1: " ++ (show (totalPriority l)))
    print ("puzzle 2: " ++ (show (sumBadges l)))
