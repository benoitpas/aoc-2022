module Day4
    (
        contains,
        overlap,
        run,
    ) where

import qualified Data.List as L

split :: Eq a => a -> [a] -> [[a]]
split c s = case L.elemIndex c s of
                Just i -> [take i s, drop (i + 1) s]
                _ -> []

getIndexes :: (Read a, Read b, Num a, Num b) => [Char] -> (a, b)
getIndexes s = case split '-' s of
                smin:smax:_ -> (read smin, read smax)
                _ -> (0,0)

iterateIntervals :: (Integer -> Integer -> Integer -> Integer -> Bool) -> String -> Bool
iterateIntervals cond l = case split ',' l of
                l1:l2:_ -> let (min1, max1) = getIndexes l1 in
                           let (min2, max2) = getIndexes l2
                           in cond min1 max1 min2 max2
                _ -> False

contains :: String -> Bool
contains = iterateIntervals (\min1 max1 min2 max2 -> (min1 <= min2 && max2 <= max1) || (min2 <= min1 && max1 <= max2))

overlap :: String -> Bool
overlap = iterateIntervals (\min1 max1 min2 max2 ->  (min1 <= max2 && min2 <= max1))

countIntervals :: (a -> Bool) -> [a] -> Int
countIntervals cond = length . (filter id) . (map cond)

run :: IO ()
run = do
    content <- readFile "src/day4_input.txt"
    let l = (lines content)
    print ("puzzle 1: " ++ (show (countIntervals contains l)))
    print ("puzzle 1: " ++ (show (countIntervals overlap l)))