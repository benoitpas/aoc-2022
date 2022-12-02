module Day2
    (
        totalScore,
        run
    ) where

score l = case l of
            "A X" -> 1 + 3
            "A Y" -> 2 + 6
            "A Z" -> 3 + 0
            "B X" -> 1 + 0
            "B Y" -> 2 + 3
            "B Z" -> 3 + 6
            "C X" -> 1 + 6
            "C Y" -> 2 + 0
            "C Z" -> 3 + 3

totalScore  l = sum (map score l)

run :: IO ()
run = do
    content <- readFile "src/day2_input.txt"
    let l = (lines content)
    print ("puzzle 1: " ++ (show (totalScore l)))