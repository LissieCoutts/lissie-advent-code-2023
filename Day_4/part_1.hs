import Data.Char (isDigit, digitToInt)

-- Card   1: 24 12 26 39 19 98 74 16 82 77 | 80 11 51  1 74 60 77 68 42 35 39 78 21 12 29 19 25 98 65 91 33 17 59 24 31
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

isSemicolonOrColonOrComma :: Char -> Bool
isSemicolonOrColonOrComma c = c == '|' || c == ':'

part_1 :: [String] -> Int
part_1 textBlob = 0

main :: IO ()
main = do
      input <- readFile "input.txt"
      let linesArray = lines input
      let answer = part_1 linesArray
      putStrLn $ "Total sum: " ++ (show $ answer)