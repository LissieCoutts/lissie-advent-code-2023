-- Game 1: 19 blue, 12 red; 19 blue, 2 green, 1 red; 13 red, 11 blue
import Data.List (insert, isSuffixOf)
import Data.Char (isDigit)

data Color = Red | Green | Blue deriving (Show)

insertAtIndex :: Int -> a -> [a] -> [a]
insertAtIndex index element list =
  let (before, after) = splitAt index list
  in before ++ [element] ++ after

-- split this on ; : and take 'tail'. Then need to check for word and match to Color. Get number from each
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

isSemicolonOrColon :: Char -> Bool
isSemicolonOrColon c = c == ';' || c == ':'

extractNumberFromString :: String -> (String, Int) -- reverse string then check for 'd' : 'e' : 'r'? maybe have seperate method for red,green,blue so I can get value for red eg
extractNumberFromString [] = ("Nothing" , 0)
extractNumberFromString ('G' : 'a' : 'm' : 'e' : rest) = ("Nothing", 0)
extractNumberFromString str
    | isSuffixOf " red" str = ("red", (read $ filter isDigit str)) -- get number and store in something
    | isSuffixOf " green" str = ("green", (read $ filter isDigit str))-- get number and store in something
    | isSuffixOf " blue" str = ("blue", (read $ filter isDigit str))  -- get number and store in something
    | otherwise = extractNumberFromString (tail str)


isPossible :: [Int] -> [Int] -> Bool
isPossible givenValues maxValues = foldr (&&) True ( zipWith (<) givenValues maxValues)

--part_1 :: Something -> Int
--part_1 blob = sum $

--main :: IO ()
--main = do
--      input <- readFile "input.txt"
--      let linesArray = lines input
--      putStrLn $ "Total sum: " ++ (show $ part_1 linesArray)

