import Text.Read (readMaybe)

data Game = Game {
    gameTime :: Int,
    recordDistance :: Int,
    numberOfWinningOutcomes :: [Int]
} deriving (Show)

parseInput :: String -> [Game]
parseInput line =
  let [timeStr, distanceStr] = lines line
      timeValues = map read (tail $ words timeStr) :: [Int]
      distanceValues = map read (tail $ words distanceStr) :: [Int]
   in zipWith (\time distance -> Game {
    gameTime = time,
    recordDistance = distance,
    numberOfWinningOutcomes = getNumberOfPossibleOutcomes time}
    ) timeValues distanceValues


getNumberOfPossibleOutcomes :: Int -> [Int]
getNumberOfPossibleOutcomes gameTime = map (\timeHeldAcceleration -> (gameTime - timeHeldAcceleration)* timeHeldAcceleration) (take (gameTime + 1) [0..])

filterGamesOnBeatsRecord :: [Int] -> Int -> [Int]
filterGamesOnBeatsRecord possibleOutcomes currentRecord = filter (> currentRecord) possibleOutcomes

part_1 :: [Game] -> [Int]
part_1 = map (\game -> length (filterGamesOnBeatsRecord (numberOfWinningOutcomes game) (recordDistance game)))

main :: IO ()
main = do
      input <- readFile "input.txt"
      let answer = product $ part_1 $ parseInput input
      putStrLn $ "Answer: " ++ show answer