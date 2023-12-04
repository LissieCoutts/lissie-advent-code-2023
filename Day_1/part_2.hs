import Data.Char (isDigit)

getNumbersFromString :: String -> String
getNumbersFromString textBlob = filter isDigit textBlob

takeFirstAndLast :: String -> Int
takeFirstAndLast numbers = read [head numbers, last numbers]

replaceWordsWithDigits :: String -> String
replaceWordsWithDigits [] = []
replaceWordsWithDigits ('o' : 'n' : 'e' : rest) = '1' : replaceWordsWithDigits ('e' : rest) -- check for eight
replaceWordsWithDigits ('t' : 'w' : 'o' : rest) = '2' : replaceWordsWithDigits ('o' : rest) -- check for one
replaceWordsWithDigits ('t' : 'h' : 'r' : 'e' : 'e' : rest) = '3' : replaceWordsWithDigits ('e' : rest) -- check for eight
replaceWordsWithDigits ('f' : 'o' : 'u' : 'r' : rest) = '4' : replaceWordsWithDigits rest
replaceWordsWithDigits ('f' : 'i' : 'v' : 'e' : rest) = '5' : replaceWordsWithDigits ('e' : rest) -- check for eight
replaceWordsWithDigits ('s' : 'i' : 'x' : rest) = '6' : replaceWordsWithDigits rest
replaceWordsWithDigits ('s' : 'e' : 'v' : 'e' : 'n' : rest) = '7' : replaceWordsWithDigits ('n' : rest) -- check for nine
replaceWordsWithDigits ('e' : 'i' : 'g' : 'h' : 't' : rest) = '8' : replaceWordsWithDigits ('t' : rest) -- check for three
replaceWordsWithDigits ('n' : 'i' : 'n' : 'e' : rest) = '9' : replaceWordsWithDigits ('e' : rest) -- check for eight
replaceWordsWithDigits (x : xs) = x : replaceWordsWithDigits xs

part_2 :: [String] -> Int
part_2 linesArray =  sum $ map (takeFirstAndLast . getNumbersFromString . replaceWordsWithDigits) linesArray

main :: IO ()
main = do
      input <- readFile "input.txt"
      let linesArray = lines input
      putStrLn $ "Total sum: " ++ (show $ part_2 linesArray)

      -- ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"]