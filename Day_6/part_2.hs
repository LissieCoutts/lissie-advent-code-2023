import Text.Read (readMaybe)
import Data.Char (isDigit)

data Game = Game {
    gameTime :: Int,
    recordDistance :: Int,
    possibleOutcomes :: [Int]
} deriving (Show)

parseInput :: String -> Game
parseInput line =
  let [timeStr, distanceStr] = lines line
      timeValue = read (filter isDigit timeStr) :: Int
      distanceValue = read (filter isDigit distanceStr) :: Int
   in  Game {
    gameTime = timeValue,
    recordDistance = distanceValue,
    possibleOutcomes = getPossibleOutcomes timeValue}


getPossibleOutcomes :: Int -> [Int]
getPossibleOutcomes gameTime = map (\timeHeldAcceleration -> (gameTime - timeHeldAcceleration)* timeHeldAcceleration)  [0 .. gameTime]

filterGamesOnBeatsRecord :: [Int] -> Int -> [Int]
filterGamesOnBeatsRecord possibleOutcomes currentRecord = filter (> currentRecord) possibleOutcomes

part_2 :: Game -> Int
part_2 game = length $ filterGamesOnBeatsRecord (possibleOutcomes game) (recordDistance game)

main :: IO ()
main = do
      input <- readFile "input.txt"
      let answer = part_2 $ parseInput input
      putStrLn $ "Answer: " ++ show answer