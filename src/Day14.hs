module Day14
    (   
        parseLines,
        run,
    ) where

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Data.Functor.Identity

lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser emptyDef

integer :: P.ParsecT String u Data.Functor.Identity.Identity Integer
integer = T.integer lexer

pairParser :: P.ParsecT String u Data.Functor.Identity.Identity (Integer, Integer)
pairParser = do
                i1 <- integer
                _ <- P.oneOf ","
                i2 <- integer
                return (i1,i2)

lineParser :: P.ParsecT String u Data.Functor.Identity.Identity [(Integer, Integer)]
lineParser = pairParser `P.sepBy1` (P.string "-> ")

parseLine :: String -> [(Integer, Integer)]
parseLine l = case P.parse lineParser "" l of
                Left err -> error ("parse error at " ++ (show err))
                Right val -> val

parseLines :: [String] -> [[(Integer, Integer)]]
parseLines = map parseLine

run :: IO ()
run = do 
    content <- readFile "src/day14_input.txt"
    let l = (lines content)
    putStrLn (show (parseLines l))
    putStrLn ("puzzle 1: ")
