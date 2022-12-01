module Day1
    ( 
        calories,
        max3Calories,
        run
    ) where

import qualified Data.List as L

calories allLines =
    let (ps,s) = foldr (\l (prevSums, sum) -> case l of
                                            "" -> (sum:prevSums, 0)
                                            l -> (prevSums, sum + read l)) ([],0) allLines
    in (s:ps)

max3Calories = sum . take 3 . reverse . L.sort

run :: IO ()
run = do
    content <- readFile "src/day1_input.txt"
    let c = calories (lines content)
    let p1 = maximum c
    print ("puzzle 1: " ++ (show p1))
    let p2 = max3Calories c
    print ("puzzle 2: " ++ (show p2))
