module Day11
    (
        Monkey(..),
        parseMonkey,
        parseMonkeys,
        nextRound,
        monkeyBusiness,
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
    show (Monkey items operation test ifTrue ifFalse) = "Monkey " ++ (show items) ++ " " ++ (show ifTrue) ++ " " ++ (show ifFalse)

instance Eq Monkey where
    (==) a b = items a == items b && ifTrue a == ifTrue b && ifFalse a == ifFalse b

parseMonkey allLines = 
    let parseList items = map (\i -> read (if last i == ',' then init i else i)) items in
    let pid l = case words (head l) of
                ["Monkey",n] -> Right ((read (init n)) :: (Int), tail l)
                _ -> Left (head l) in
    let startingItems l = case words (head l) of
                            "Starting":"items:":items -> Right (parseList items, tail l)
                            _ -> Left (head l) in
    let operation l = case words (head l) of
                        ["Operation:","new", "=","old","*","old"] -> Right (\x -> x*x, tail l)
                        ["Operation:","new", "=","old","*",n] -> Right (let nv = read n in (*nv), tail l)
                        ["Operation:","new", "=","old","+",n] -> Right (let nv = read n in (+nv), tail l)
                        _ -> Left (head l) in
    let test l = case words (head l) of
                    ["Test:", "divisible", "by", n] -> Right  (let nv = read n in (\x -> x `mod` nv == 0),tail l)
                    _ -> Left (head l) in
    let ifTrue l = case words (head l) of
                    ["If", "true:", "throw", "to", "monkey", n] -> Right (read n, tail l)
                    _ -> Left (head l) in
    let ifFalse l = case words (head l) of
                        ["If","false:","throw","to", "monkey", n] -> Right (read n, tail l)
                        _ -> Left (head l)

    in (pid  allLines) >>=  
                (\(pid,l2)   -> (startingItems l2) >>= 
                (\(items,l3) -> ((operation l3) >>= 
                (\(op,l4)    -> (test l4) >>= 
                (\(tst, l5)  -> (ifTrue l5) >>= 
                (\(ift, l6)  -> (ifFalse l6) >>= 
                (\(iff, l7)  -> return (pid,Monkey items op tst ift iff ,l7))))))))

parseMonkeys allLines = 
    let allLinesF = filter (\l -> length l > 0) allLines in
    let (_,monkeyMap) = until (\(l,_) -> length l == 0) (\(l,m) -> case parseMonkey l of
                                                                        Right (pid, monkey, l2) -> (l2, M.insert pid (0,monkey) m)) (allLinesF, M.empty)
    in monkeyMap

nextRound :: M.Map Int (Int, Monkey) -> M.Map Int (Int, Monkey)
nextRound monkeyMap = 
    foldl (\m i ->let (c,mk) = m M.! i in
                  let (nmk, throws) =  nextMonkey mk
                  in updateMap (M.insert i (c+length throws,nmk) m) throws) monkeyMap (L.sort (M.keys monkeyMap))
    where
        nextMonkey mk = (mk { items = [] }, map (\i ->  let ni = (operation mk i ) `div` 3 
                                                        in (if test mk ni then ifTrue mk 
                                                                          else ifFalse mk, ni)) (items mk))
        updateMap mkMap throws = foldl (\mm (mki,w) -> let (c,mk) = (mm M.! mki) in
                                                       let nmk = mk { items = items mk ++ [w]}
                                                       in M.insert mki (c,nmk) mm) mkMap throws

monkeyBusiness n allLines =
    let lmap = last $ take (n+1) (iterate nextRound (parseMonkeys allLines)) in
    let l = L.sort $ map fst (M.elems lmap)
    in foldr (*) 1 (drop (length l - 2) l)

run :: IO ()
run = do
    content <- readFile "src/day11_input.txt"
    let l = (lines content)

    putStrLn ("puzzle 1: " ++ (show  (monkeyBusiness 20 l)))
