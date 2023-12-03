-- Problem: Find elf that has the most calories.
import Data.List.Split (splitOn)


-- If this were procedural:
-- Import txt file
-- read file and covert to list of str or something, seperated by lines
-- add together values of calories and if theres a break, start new elf
-- return the elf that had the most calories (by index?)
str input = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"



-- Function to find the calories for an elf:
totalCalories: [Int] -> Int
totalCalories : sum

-- Function to find the max calories:
maxCalories : [Int] -> Int
maxCalories : max