module Day13
    (
        Packet(..),
        toTokens,
        parse,
        comparePacket,
        compareString,
        sumIndexGoodPairs,
        run,
    ) where

import qualified Data.Char as C
import qualified Data.List as L

data Packet = One Int | List [Packet] deriving (Eq, Show)

toTokens = L.groupBy (\c1 c2 -> (C.isDigit c1) && (C.isDigit c2))

parse :: [String] -> ([String], Packet, Bool)
parse tokens =
    let addValue p v = case p of
                        List l -> List (l ++ [v]) in

    let next (t,p,_) = case t of
                            ",":r -> (r,p, False)
                            "[":r -> let (nr,ep,_) = parse r
                                     in (nr, addValue p ep, False)
                            "]":r -> (r,p,True)
                            vs:r -> let v = (read vs) :: Int
                                    in (r, addValue p (One v), False)
                            [] -> ([],p,True) in
    until (\(t,_,f) -> length t == 0 || f) next (tokens, List [], False)

comparePacket :: Packet -> Packet -> Maybe Bool
comparePacket t1 t2 = 
    let compareList left right =  case (left,right) of
                                    ([],[])       -> Nothing
                                    ([],_)        -> Just True
                                    (_,[])        -> Just False
                                    (hl:rl,hr:rr) -> case comparePacket hl hr of
                                                        Nothing -> compareList rl rr
                                                        v -> v
    in case (t1,t2) of
                    (One left, One right) -> if left == right then Nothing
                                                               else Just (left < right)
                    (One left, List right) -> compareList [One left] right
                    (List left, One right) -> compareList left [One right]
                    (List left, List right) -> compareList left right

compareString s1 s2 =
    let (_, p1, _) = parse (toTokens s1)
        (_, p2,_ ) = parse $ toTokens s2
    in comparePacket p1 p2

sumIndexGoodPairs :: [String] -> Int
sumIndexGoodPairs l = 
    let next (l,pairs) = case l of
                            "":r -> (r,pairs)
                            left:right:r -> (r,pairs ++ [(left,right)]) in
    let (_, p) = until (\(l,_) -> length l == 0) next (l,[])
    in sum $ map snd (filter (\((l,r),_) -> (compareString l r) == Just True) (p `zip` [1..]))

run :: IO ()
run = do 
    content <- readFile "src/day13_input.txt"
    let l = (lines content)

    putStrLn ("puzzle 1: " ++ show (sumIndexGoodPairs l))