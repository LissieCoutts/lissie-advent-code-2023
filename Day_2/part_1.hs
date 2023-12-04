-- Game 1: 19 blue, 12 red; 19 blue, 2 green, 1 red; 13 red, 11 blue
import Data.List (insert, isSuffixOf, sortBy)
import Data.Char (isDigit)
import Data.Map.Strict (Map, insertWith, empty, toList)
import Data.List (foldl')
import Data.Function (on)


wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

isSemicolonOrColonOrComma :: Char -> Bool
isSemicolonOrColonOrComma c = c == ';' || c == ':' || c == ','

extractColourAndNumberFromString :: String -> (String, Int)
extractColourAndNumberFromString [] = ("Nothing" , 0)
extractColourAndNumberFromString ('G' : 'a' : 'm' : 'e' : rest) = ("Nothing", 0)
extractColourAndNumberFromString str
    | isSuffixOf " red" str = ("red", (read $ filter isDigit str))
    | isSuffixOf " green" str = ("green", (read $ filter isDigit str))
    | isSuffixOf " blue" str = ("blue", (read $ filter isDigit str))
    | otherwise = ("Nothing" , 0)

accumulateMaxValues :: [(String, Int)] -> [(String, Int)]
accumulateMaxValues tuples = toList $ foldr (\(colour, value) acc -> insertWith max colour value acc) empty tuples

sortAndExtractNumbers :: [(String, Int)] -> [Int]
sortAndExtractNumbers tuples = tail $ map snd $ sortBy (on compare fst) tuples

isPossible :: [Int] -> [Int] -> Bool
isPossible givenValues maxValues = foldr (&&) True ( zipWith (<) givenValues maxValues)

game_is_possible :: String -> [Int] -> Bool
game_is_possible givenValues maxValues = isPossible (sortAndExtractNumbers $ accumulateMaxValues $ map extractColourAndNumberFromString (wordsWhen isSemicolonOrColonOrComma givenValues)) [20,20,20]

map_lines_to_bool :: [String] -> [Bool]
map_lines_to_bool linesArray = map (\line -> game_is_possible line [14,13,12]) linesArray

map_bools_to_index :: [Bool] -> [Int]
map_bools_to_index bools = map (\(index, value) -> if value then (index + 1) else 0) (zip [0..] bools)

part_1 :: [String] -> Int
part_1 linesArray = sum $ (map_bools_to_index . map_lines_to_bool) linesArray

main :: IO ()
main = do
      input <- readFile "input.txt"
      let linesArray = lines input
      let answer = part_1 linesArray
      putStrLn $ "Total sum: " ++ (show $ answer)

