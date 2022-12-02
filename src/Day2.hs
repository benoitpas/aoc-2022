module Day2
    (
        score1,
        score2,
        totalScore,
        run
    ) where

score1 l = case l of
            "A X" -> 1 + 3
            "A Y" -> 2 + 6
            "A Z" -> 3 + 0
            "B X" -> 1 + 0
            "B Y" -> 2 + 3
            "B Z" -> 3 + 6
            "C X" -> 1 + 6
            "C Y" -> 2 + 0
            "C Z" -> 3 + 3

score2 l = case l of
            "A X" -> 3 + 0
            "A Y" -> 1 + 3
            "A Z" -> 2 + 6
            "B X" -> 1 + 0
            "B Y" -> 2 + 3
            "B Z" -> 3 + 6
            "C X" -> 2 + 0
            "C Y" -> 3 + 3
            "C Z" -> 1 + 6

totalScore s l = sum (map s l)

run :: IO ()
run = do
    content <- readFile "src/day2_input.txt"
    let l = (lines content)
    print ("puzzle 1: " ++ (show (totalScore score1 l)))
    print ("puzzle 2: " ++ (show (totalScore score2 l)))