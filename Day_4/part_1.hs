import Data.Char (isDigit, digitToInt)

data Card = Card
    { cardNumber :: Int
    , winningNumbers :: [Int]
    , chosenNumbers :: [Int]
    } deriving (Show)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

isSemicolonOrColonOrComma :: Char -> Bool
isSemicolonOrColonOrComma c = c == '|' || c == ':'

parseCard :: [String] -> Card
parseCard strings = Card
    { cardNumber = read $ filter isDigit (head strings)  -- Assuming the first string represents the card number
    , winningNumbers = map read (words (strings !! 1))  -- Assuming the second string contains space-separated winning numbers
    , chosenNumbers = map read (words (strings !! 2))  -- Assuming the third string contains space-separated card numbers
    }

intersect :: [Int] -> [Int] -> [Int]
intersect [] _ = []
intersect _ [] = []
intersect xs ys = filter (\y -> elem y xs) ys

numberOfWinningNumbersForCard :: Card -> Int
numberOfWinningNumbersForCard card = length $ intersect (chosenNumbers card) (winningNumbers card)

getValueForCard :: String -> Int
getValueForCard line
    | numberOfWinningNumbersForCard (parseCard $ wordsWhen isSemicolonOrColonOrComma line) > 0  = 2^ (numberOfWinningNumbersForCard (parseCard $ wordsWhen isSemicolonOrColonOrComma line) - 1)
    | otherwise = 0

part_1 :: [String] -> Int
part_1 input = sum $ map getValueForCard input

main :: IO ()
main = do
      input <- readFile "input.txt"
      let linesArray = lines input
      let answer = part_1 linesArray
      putStrLn $ "Total sum: " ++ show answer