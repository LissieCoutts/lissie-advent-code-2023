import Data.Char (isDigit)

getNumberFromArray :: String -> String
getNumberFromArray textBlob = filter isDigit textBlob

takeFirstAndLast :: String -> Int
takeFirstAndLast numbers = read [head numbers, last numbers]

replaceSingleDigits :: String -> String
replaceSingleDigits [] = []
replaceSingleDigits ('o' : 'n' : 'e' : rest) = '1' : replaceSingleDigits rest
replaceSingleDigits ('t' : 'w' : 'o' : rest) = '2' : replaceSingleDigits rest
replaceSingleDigits ('t' : 'h' : 'r' : 'e' : 'e' : rest) = '3' : replaceSingleDigits rest
replaceSingleDigits ('f' : 'o' : 'u' : 'r' : rest) = '4' : replaceSingleDigits rest
replaceSingleDigits ('f' : 'i' : 'v' : 'e' : rest) = '5' : replaceSingleDigits rest
replaceSingleDigits ('s' : 'i' : 'x' : rest) = '6' : replaceSingleDigits rest
replaceSingleDigits ('s' : 'e' : 'v' : 'e' : 'n' : rest) = '7' : replaceSingleDigits rest
replaceSingleDigits ('e' : 'i' : 'g' : 'h' : 't' : rest) = '8' : replaceSingleDigits rest
replaceSingleDigits ('n' : 'i' : 'n' : 'e' : rest) = '9' : replaceSingleDigits rest
replaceSingleDigits (x : xs) = x : replaceSingleDigits xs

part_2 :: [String] -> Int
part_2 linesArray =  sum $ map (takeFirstAndLast . getNumberFromArray . replaceSingleDigits) linesArray

main :: IO ()
main = do
      input <- readFile "input.txt"
      let linesArray = lines input
      putStrLn $ "Total sum: " ++ (show $ part_2 linesArray)