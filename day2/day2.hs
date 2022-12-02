import Data.List
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStrLn $ "Part One score is: " ++ part1 contents
  putStrLn $ "Part Two score is: " ++ part2 contents

part1 :: String -> String
part1 = show . sum . getScores . convertToInts . splitList

part2 :: String -> String
part2 = show . sum . partTwoScores . convertToInts . splitList

-- Convert input string to list of tuples for each match
splitList :: String -> [(String, String)]
splitList = map (tuplify . words) . lines
  where
    tuplify [x, y] = (x, y)

-- Map letters to RPS values
convertToInts :: [(String, String)] -> [(Integer, Integer)]
convertToInts = map (convertElves . convertMe)
  where
    convertElves (elf, me)
      | elf == "A" = (1, me)
      | elf == "B" = (2, me)
      | elf == "C" = (3, me)
    convertMe (elf, me)
      | me == "X" = (elf, 1)
      | me == "Y" = (elf, 2)
      | me == "Z" = (elf, 3)

-- Get score for each match
getScores :: [(Integer, Integer)] -> [Integer]
getScores = map scorer
  where
    scorer (elf, me)
      -- handle edge cases
      | elf == 1 && me == 3 = 3
      | elf == 3 && me == 1 = 1 + 6
      | me > elf = me + 6
      | me == elf = me + 3
      | me < elf = me

-- Calculate score based on elf's choice and if we want to win or lose
partTwoScores :: [(Integer, Integer)] -> [Integer]
partTwoScores = map scorer
  where
    scorer :: (Integer, Integer) -> Integer
    scorer (elf, me)
      | me == 1 = lose elf
      | me == 2 = draw elf
      | me == 3 = win elf
      where
        lose elf
          | elf == 1 = 3
          | otherwise = elf - 1
        draw elf = elf + 3
        win elf
          | elf == 3 = 1 + 6
          | otherwise = elf + 1 + 6
