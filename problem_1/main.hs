import Data.Char (isDigit, digitToInt)

getNumberFromArray :: String -> String -- Returns string of the numbers on that line
getNumberFromArray textBlob = filter isDigit textBlob

takeFirstAndLast :: String -> String -- Takes the first and last item from a string
takeFirstAndLast numbers = [head numbers, last numbers]

getFirstAndLast :: String -> Int -- Sums first and last digits
getFirstAndLast numbers = read $ takeFirstAndLast numbers

main :: IO ()
main = do
      input <- readFile "input.txt"
      let linesArray = lines input
      let totalSum = sum $ map (getFirstAndLast . getNumberFromArray) linesArray
      putStrLn $ "Total sum: " ++ show totalSum
      -- mapM_ (putStrLn . show . getFirstAndLast . getNumberFromArray) linesArray