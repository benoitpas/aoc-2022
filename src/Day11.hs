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
    divisor :: Int,
    ifTrue :: Int,
    ifFalse :: Int
}

instance Show Monkey where
    show (Monkey its _ dvs ifT ifF) = "Monkey " ++ (show its) ++ " " ++ (show dvs) ++ " " ++ (show ifT) ++ " " ++ (show ifF)

instance Eq Monkey where
    (==) a b = items a == items b && ifTrue a == ifTrue b && ifFalse a == ifFalse b

parseMonkey allLines = 
    let parseList = map (\i -> read (if last i == ',' then init i else i)) in
    let pPid (h:r) = case words h of
                        ["Monkey",n] -> Right ((read (init n)) :: (Int), r)
                        _   -> Left h in
    let pStartingItems (h:r) = case words h of
                            "Starting":"items:":its -> Right (parseList its, r)
                            _ -> Left h in
    let pOperation (h:r) = case words h of
                        ["Operation:","new", "=","old","*","old"] -> Right (\x -> x*x, r)
                        ["Operation:","new", "=","old","*",n] -> Right (let nv = read n in (*nv), r)
                        ["Operation:","new", "=","old","+",n] -> Right (let nv = read n in (+nv), r)
                        _ -> Left h in
    let pTest (h:r) = case words h of
                    ["Test:", "divisible", "by", n] -> Right  (read n, r)
                    _ -> Left h in
    let pIfTrue (h:r) = case words h of
                        ["If", "true:", "throw", "to", "monkey", n] -> Right (read n, r)
                        _ -> Left h in
    let pIfFalse (h:r) = case words h of
                        ["If","false:","throw","to", "monkey", n] -> Right (read n, r)
                        _ -> Left h

    in (pPid  allLines) >>=
                (\(pid,l2)   -> (pStartingItems l2) >>=
                (\(its,l3)   -> (pOperation l3) >>=
                (\(op,l4)    -> (pTest l4) >>=
                (\(tst, l5)  -> (pIfTrue l5) >>=
                (\(ift, l6)  -> (pIfFalse l6) >>=
                (\(iff, l7)  -> return (pid,Monkey its op tst ift iff ,l7)))))))

parseMonkeys allLines = 
    let allLinesF = filter (\l -> length l > 0) allLines in
    let (_,monkeyMap) = until   (\(l,_) -> length l == 0)
                                (\(l,m) -> case parseMonkey l of
                                            Right (pid, monkey, l2) -> (l2, M.insert pid (0,monkey) m)) (allLinesF, M.empty)
    in monkeyMap

nextRound :: M.Map Int (Int, Monkey) -> M.Map Int (Int, Monkey)
nextRound monkeyMap = 
    foldl (\m i ->let (c,mk) = m M.! i in
                  let (nmk, throws) =  nextMonkey mk
                  in updateMap (M.insert i (c+length throws,nmk) m) throws) monkeyMap (L.sort (M.keys monkeyMap))
    where
        nextMonkey mk = (mk { items = [] }, map (\i ->  let ni = (operation mk i ) `div` 3 
                                                        in (if  ni `mod` (divisor mk) == 0   then ifTrue mk 
                                                                                        else ifFalse mk, ni)) 
                                            (items mk))
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
