module Day11
    (
        Monkey(..),
        parseMonkey,
        run,
    ) where

import qualified Data.List as L
import qualified Data.Map as M

data Monkey = Monkey {
    items :: [Int],
    operation :: Int -> Int,
    test :: Int -> Bool,
    ifTrue :: Int,
    ifFalse :: Int
}

instance Show Monkey where
    show (Monkey items _ _ ifTrue ifFalse) = "Monkey " ++ (show items) ++ " " ++ (show ifTrue) ++ " " ++ (show ifFalse)

instance Eq Monkey where
    (==) a b = items a == items b && ifTrue a == ifTrue b && ifFalse a == ifFalse b

parseMonkey allLines = 
    let parseList items = map (\i -> read (if last i == ',' then init i else i)) items in
    let pid l = case words (head l) of
                ["Monkey",n] -> Just ((read (init n)) :: (Int), tail l)
                _ -> Nothing in
    let startingItems l = case words (head l) of
                            "Starting":"items:":items -> Just (parseList items, tail l)
                            _ -> Nothing in
    let operation l = case words (head l) of
                        ["Operation:","new", "=","old","*",n] -> Just (let nv = read n in (*nv),tail l)
                        _ -> Nothing in
    let test l = case words (head l) of
                    ["Test:", "divisible", "by", n] -> Just  (let nv = read n in (\x -> x `mod` nv == 0),tail l)
                    _ -> Nothing in
    let ifTrue l = case words (head l) of
                    ["If", "true:", "throw", "to", "monkey", n] -> Just (read n, tail l)
                    _ -> Nothing in
    let ifFalse l = case words (head l) of
                        ["If","false:","throw","to", "monkey", n] -> Just (read n, tail l)
                        _ -> Nothing

    in (pid  allLines) >>=  
                (\(pid,l2)   -> (startingItems l2) >>= 
                (\(items,l3) -> ((operation l3) >>= 
                (\(op,l4)    -> (test l4) >>= 
                (\(tst, l5)  -> (ifTrue l5) >>= 
                (\(ift, l6)  -> (ifFalse l6) >>= 
                (\(iff, l7)  -> return (pid,Monkey items op tst ift iff ,tail l7))))))))

 --   (pid allLines) >>=  (\(pid,l2)   -> (startingItems l2) >>=
 --                       (\(items,l3) -> (Just (pid,items,l3))))

run :: IO ()
run = do
    content <- readFile "src/day11_input.txt"
    let l = (lines content)
    let (_,monkeyMap) = until (\(ll,_) -> length l == 0) (\(ll,m) -> case parseMonkey ll of
                                                                        Just (pid, monkey, ll2) -> (ll2, M.insert pid monkey m)) (l,M.empty)
    putStrLn ("puzzle 1: " ++ (show ( monkeyMap)))
