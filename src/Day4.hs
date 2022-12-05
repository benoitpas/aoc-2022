module Day4
    (
        contains,
        run,
    ) where

import qualified Data.List as L

split c s = case L.elemIndex c s of
                Just i -> [take i s, drop (i + 1) s]
                _ -> []

getIndexes s = case split '-' s of
                smin:smax:_ -> (read smin, read smax)
                _ -> (0,0)

contains :: String -> Bool
contains l = case split ',' l of
                l1:l2:_ -> let (min1, max1) = getIndexes l1 in
                           let (min2, max2) = getIndexes l2
                           in (min1 <= min2 && max2 <= max1) || (min2 <= min1 && max1 <= max2)
                _ -> False

run :: IO ()
run = do
    content <- readFile "src/day4_input.txt"
    let l = (lines content)
    print ("puzzle 1: " ++ (show ( length (filter id (map contains l)))))