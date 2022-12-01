module Day1
    ( 
        maxCalories,
        run
    ) where


maxCalories allLines = 
    let (ps,s) = foldr (\l (prevSums, sum) -> case l of
                                            "" -> (sum:prevSums, 0)
                                            l -> (prevSums, sum + read l)) ([],0) allLines
    in foldr max 0 (s:ps)

run :: IO ()
run = do
    content <- readFile "src/day1_input.txt"
    print ("puzzle 1: " ++ (show (maxCalories (lines content))))
