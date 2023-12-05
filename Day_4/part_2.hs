import Data.Char (isDigit, digitToInt)

data Card = Card
    { cardNumber :: Int
    , winningNumbers :: [Int]
    , chosenNumbers :: [Int]
    , currentCopiesOfCard :: Int
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
    , currentCopiesOfCard = 1
    }

intersect :: [Int] -> [Int] -> [Int]
intersect [] _ = []
intersect _ [] = []
intersect xs ys = filter (\y -> elem y xs) ys

numberOfWinningNumbersForCard :: Card -> Int
numberOfWinningNumbersForCard card = length $ intersect (chosenNumbers card) (winningNumbers card)

-- build an array of ints. For some index i, add (instances of card) * numberOfWinningNumbersForCard[i] foreach
-- index i + 1 until i + numberOfWinningNumbersForCard[i]  

getCard :: String -> Card
getCard line = parseCard $ wordsWhen isSemicolonOrColonOrComma line

part_2 :: [String] -> [Card]
part_2 input =  map (\line ) getCard input

main :: IO ()
main = do
      input <- readFile "input.txt"
      let linesArray = lines input
      let answer = part_2 linesArray
      putStrLn $ "Total sum: " ++ show answer