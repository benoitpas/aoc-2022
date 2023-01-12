module Day14
    (   
        parseLines,
        addRocks,
        nbSandGrains,
        run,
    ) where

import qualified Data.List as L
import qualified Data.Map as M

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Data.Functor.Identity

type Point = (Integer, Integer)

lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser emptyDef

integer :: P.ParsecT String u Data.Functor.Identity.Identity Integer
integer = T.integer lexer

pairParser :: P.ParsecT String u Data.Functor.Identity.Identity Point
pairParser = do
                i1 <- integer
                _ <- P.oneOf ","
                i2 <- integer
                return (i1,i2)

lineParser :: P.ParsecT String u Data.Functor.Identity.Identity [Point]
lineParser = pairParser `P.sepBy1` (P.string "-> ")

parseLine :: String -> [Point]
parseLine l = case P.parse lineParser "" l of
                Left err -> error ("parse error at " ++ (show err))
                Right val -> val

parseLines :: [String] -> [[Point]]
parseLines = map parseLine

intSeq beg end
    | beg <= end = [beg .. end]
    | True = [beg,beg-1 .. end]

generatePoints (x1,y1) (x2,y2)
    | x1 == x2 = zip (repeat x1) (intSeq y1 y2)
    | True = zip (intSeq x1 x2) (repeat y1)

addRocks :: M.Map Point Char -> [Point] -> M.Map Point Char
addRocks m rocks = 
    let (_,r) = foldl (\(p',m') p -> foldl (\(_,m'') np -> (np,M.insert np '#' m'')) (p,m') (generatePoints p' p))
                      ((head rocks),m) (tail rocks)
    in r

iPSand :: Point
iPSand = (500,0)

next maxY isFloor (cave, (xps,yps), _) =
    let nYps = yps + 1 in
    case (getPoint (xps,yps), getPoint (xps-1,nYps), getPoint (xps,nYps), getPoint (xps+1,nYps)) of
        ('o', _ , _ , _ ) -> (cave, (xps,yps), True)
        ( _ , _ ,' ', _ ) -> (cave, (xps,nYps), not isFloor && maxY < nYps)
        ( _ ,' ', _ , _ ) -> (cave, (xps-1,nYps), not isFloor && maxY < nYps)
        ( _ , _ , _ ,' ') -> (cave, (xps+1,nYps), not isFloor && maxY < nYps)
        _ -> (M.insert (xps,yps) 'o' cave, iPSand, False)
    where getPoint (x,y) = if isFloor && y == (maxY + 2) then '#' else M.findWithDefault ' ' (x,y) cave

nbSandGrains isFloor l =
    let rocks = parseLines l in
    let terrain = foldl addRocks M.empty rocks in
    let maxY = L.maximum $ map snd (M.keys terrain) in
    let (c,_,_) = until (\(_,_,f) -> f) (next maxY isFloor) (terrain, iPSand, False)
    in length $ filter (== 'o') (M.elems c)

run :: IO ()
run = do 
    content <- readFile "src/day14_input.txt"
    let l = (lines content)
    putStrLn ("puzzle 1: " ++ (show (nbSandGrains False l)))
    putStrLn ("puzzle 2: " ++ (show (nbSandGrains True l)))
