part_1 :: [String] -> Int
part_1 linesArray =  sum $ map (takeFirstAndLast . getNumberFromArray) linesArray

main :: IO ()
main = do
      input <- readFile "input.txt"
      let linesArray = lines input
      putStrLn $ "Total sum: " ++ (show $ part_1 linesArray)
