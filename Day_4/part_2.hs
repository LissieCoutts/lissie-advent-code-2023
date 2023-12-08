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

getCard :: String -> Card
getCard line = parseCard $ wordsWhen isSemicolonOrColonOrComma line

getCopiesOfCards:: [Card] -> [Int]
getCopiesOfCards = map currentCopiesOfCard


addOneToFirstValuesInList :: (Ord a, Num a, Enum a) => a -> Int -> [Card] -> [Card]
addOneToFirstValuesInList i numberToAdd = zipWith
    (\index card -> if index < i then addCopiesToCard card numberToAdd else card) [0..]

addCopiesToCard :: Card -> Int -> Card
addCopiesToCard card toBeAdded =  card { currentCopiesOfCard = currentCopiesOfCard card + toBeAdded }

addCopies :: [Card] -> [Card]
addCopies [] = []
addCopies (x : rest) = x : addCopies (addOneToFirstValuesInList ( numberOfWinningNumbersForCard x) (currentCopiesOfCard x) rest)

numberOfWinningNumbersArray :: [String] -> [Int]
numberOfWinningNumbersArray = map (numberOfWinningNumbersForCard . getCard)

part_2 :: [String] -> Int
part_2 linesArray = sum $ (getCopiesOfCards . addCopies . map getCard) linesArray

main :: IO ()
main = do
      input <- readFile "input.txt"
      let linesArray = lines input
      let answer = part_2 linesArray
      putStrLn $ "Total sum: " ++ show answer