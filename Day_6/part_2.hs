import Data.Char (isDigit)

getNumberOfPossibleOutcomes :: Int -> Int -> Int
getNumberOfPossibleOutcomes t s = t - 1 - 2 * floor (fromIntegral t/2 - 0.5 * sqrt (fromIntegral t^2 - 4 * fromIntegral s))

part_2 :: String -> IO ()
part_2 input =
  let [timeStr, distanceStr] = lines input
      time = read (filter isDigit timeStr) :: Int
      distance = read (filter isDigit distanceStr) :: Int
      result = getNumberOfPossibleOutcomes time distance
    in putStrLn $ "Answer: " ++ show result

main :: IO ()
main = do
      input <- readFile "input.txt"
      part_2 input