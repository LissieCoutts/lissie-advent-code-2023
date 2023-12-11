import Text.Read (readMaybe)
import Data.Char (isDigit)

data Game = Game {
    gameTime :: Float,
    recordDistance :: Float,
    numberOfWinningOutcomes :: Float
} deriving (Show)

parseInput :: String -> Game
parseInput line =
  let [timeStr, distanceStr] = lines line
      timeValue = read (filter isDigit timeStr) :: Float
      distanceValue = read (filter isDigit distanceStr) :: Float
   in  Game {
    gameTime = timeValue,
    recordDistance = distanceValue,
    numberOfWinningOutcomes = getNumberOfPossibleOutcomes timeValue distanceValue }


getNumberOfPossibleOutcomes :: Float -> Float -> Float
getNumberOfPossibleOutcomes t y = sqrt (t^2 - 4*y) - 1

part_2 :: Game -> Float
part_2 = numberOfWinningOutcomes

main :: IO ()
main = do
      input <- readFile "input.txt"
      let answer = part_2 $ parseInput input
      putStrLn $ "Answer: " ++ show answer