module Day7
    (
        File(..),
        Directory(..),
        parseLs,
        parseDirectories,
        parseDirectory,
        parse,
        puzzle1,
        run,
    ) where

import qualified Data.List as L

data Directory = Directory
    { dName :: String,
      dSize :: Int,
      dFiles      :: [File],
      dDirectories :: [Directory] 
    } deriving (Eq, Show)

data File = File
    { fName :: String,
      fSize :: Int
    } deriving (Eq, Show)

parseLs l = let (fileLines, r) = L.span (\l -> (head l) /= '$') l in
            let files =  fileLines >>=  (\l -> case words l of
                                                size:name:_ -> if (head size) <= '9' then [File name (read size)] else [])
            in (files, r)

parseDirectories l = let next (dirs,r,f) = case r of 
                                            h:r2 -> case words h of
                                                        ["$","cd",".."] -> (dirs,r2,True)
                                                        ["$","cd", name] -> let (dir, r3) = parseDirectory name r2 in (dir:dirs,r3, length r3 == 0)
                                            _ -> (dirs, r, True) in
                     let (dirs,r,_) = until (\(_,_,f) -> f) next ([],l,False)
                     in (dirs,r)

parseDirectory name l = let (files,l2) = case words (head l) of
                                                ["$","ls"] -> parseLs (tail l) in
                        let (directories, l3) = parseDirectories l2 in
                        let size = sum $ (map fSize files ++ map dSize directories)
                        in (Directory name size files directories, l3)

parse l = case words (head l) of
            ["$","cd", "/"] -> let (t,_) = parseDirectory "." (tail l) in t

puzzle1 l = let t = parse l in
            let next t = let s = dSize t 
                         in (dDirectories t >>= next) ++ if s <= 100000 then [s] else []
            in sum $ next t

run :: IO ()
run = do
    content <- readFile "src/day7_input.txt"
    let l = (lines content)
    print ("puzzle 1: " ++ (show (puzzle1 l)))
