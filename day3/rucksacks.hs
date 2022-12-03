import Data.List
import Data.Maybe
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStrLn $ "Part one is: " ++ show (partOne contents)
  putStrLn $ "Part two is: " ++ show (partTwo contents)

-- This should probably use a sum like part 2 but I gave up on fixing type errors to make that work
partOne :: String -> Int
partOne input_string =
  let rucksacks = map splitMiddle $ lines input_string
      value = getValue . findOverlap
   in foldl (\acc x -> acc + value x) 0 rucksacks

partTwo :: String -> Int
partTwo input_string =
  let badgeValues = map getValue . findBadges
   in sum . badgeValues $ lines input_string

-- Get the value of a character
getValue :: String -> Int
getValue item =
  let itemPriorities = ['a' .. 'z'] ++ ['A' .. 'Z']
   in fromMaybe 0 (head item `elemIndex` itemPriorities) + 1

-- Split a string into two parts
splitMiddle :: String -> (String, String)
splitMiddle rucksack =
  let midpoint = length rucksack `div` 2
   in splitAt midpoint rucksack

-- find overlapping characters of two strings
findOverlap :: (String, String) -> String
findOverlap (rucksackA, rucksackB) = rucksackA `intersect` rucksackB

-- Ha! Recursive functions :) find overlaps of each set of three strings
findBadges :: [String] -> [String]
findBadges (x : y : z : xs)
  | null xs = [findGroupBadge]
  | otherwise = findGroupBadge : findBadges xs
  where
    findGroupBadge = nub (findOverlap (x, y) `intersect` findOverlap (y, z))
