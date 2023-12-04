import Data.Char (isDigit)

getNumbersFromString :: String -> String
getNumbersFromString textBlob = filter isDigit textBlob

takeFirstAndLast :: String -> Int
takeFirstAndLast numbers = read [head numbers, last numbers]

-- The problem here is that it works left to right. If the last number on the line is for example twone, then do we want "2" or "1" ?
replaceWordsWithDigits :: String -> String
replaceWordsWithDigits [] = []
replaceWordsWithDigits ('o' : 'n' : 'e' : rest) = '1' : replaceWordsWithDigits rest
replaceWordsWithDigits ('t' : 'w' : 'o' : rest) = '2' : replaceWordsWithDigits rest
replaceWordsWithDigits ('t' : 'h' : 'r' : 'e' : 'e' : rest) = '3' : replaceWordsWithDigits rest
replaceWordsWithDigits ('f' : 'o' : 'u' : 'r' : rest) = '4' : replaceWordsWithDigits rest
replaceWordsWithDigits ('f' : 'i' : 'v' : 'e' : rest) = '5' : replaceWordsWithDigits rest
replaceWordsWithDigits ('s' : 'i' : 'x' : rest) = '6' : replaceWordsWithDigits rest
replaceWordsWithDigits ('s' : 'e' : 'v' : 'e' : 'n' : rest) = '7' : replaceWordsWithDigits rest
replaceWordsWithDigits ('e' : 'i' : 'g' : 'h' : 't' : rest) = '8' : replaceWordsWithDigits rest
replaceWordsWithDigits ('n' : 'i' : 'n' : 'e' : rest) = '9' : replaceWordsWithDigits rest
replaceWordsWithDigits (x : xs) = x : replaceWordsWithDigits xs

part_2 :: [String] -> Int
part_2 linesArray =  sum $ map (takeFirstAndLast . getNumbersFromString . replaceWordsWithDigits) linesArray

main :: IO ()
main = do
      input <- readFile "input.txt"
      let linesArray = lines input
      putStrLn $ "Total sum: " ++ (show $ part_2 linesArray)

      -- ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"]