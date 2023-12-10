

data Game = Game {
    gameTime :: Int,
    recordDistance :: Int,
    possibleOutcomes :: [Int]
} deriving (Show)

getPossibleOutcomes :: Int -> [Int]
getPossibleOutcomes gameTime = zipWith (\timeHeldAcceleration -> (gameTime - timeHeldAcceleration)* timeHeldAcceleration) take (gameTime + 1) [0..]


part_1 :: [String] -> Int
part_1 input = 5

main :: IO ()
main = do
      input <- readFile "input.txt"
      let linesArray = lines input
      let answer = part_1 linesArray
      putStrLn $ "Total sum: " ++ show answer

--Time:        54     81     70     88
--Distance:   446   1292   1035   1007