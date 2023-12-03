import Data.List.Split (splitOn)

-- Function to calculate the total Calories carried by an Elf
calculateTotalCalories :: [Int] -> Int
calculateTotalCalories = sum

-- Function to find the Elf carrying the most Calories
findMaxCaloriesElf :: [[Int]] -> (Int, Int)
findMaxCaloriesElf = maximumBy (\(_,calories1) (_,calories2) -> compare calories1 calories2) . zip [1..] . map calculateTotalCalories

main :: IO ()
main = do
    -- Read input until an empty line is encountered
    input <- getContents
    let elvesData = map (map read . lines) (splitOn "\n\n" input)

    -- Find the Elf with the most Calories
    let (elfNumber, maxCalories) = findMaxCaloriesElf elvesData

    putStrLn $ "Elf #" ++ show elfNumber ++ " is carrying the most Calories: " ++ show maxCalories