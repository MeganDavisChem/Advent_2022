import Data.List
import Data.Maybe
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  --  putStrLn $ "Part one is: " ++ show $ partOne contents
  putStrLn $ "Part one is: " ++ show (partOne contents)
  putStrLn $ "Part two is: " ++ show (partTwo contents)

-- Get the value of a character
getValue :: Char -> Int
getValue item =
  let itemPriorities = ['a' .. 'z'] ++ ['A' .. 'Z']
   in fromMaybe 0 (item `elemIndex` itemPriorities) + 1

-- Split a string into two parts
splitMiddle :: String -> (String, String)
splitMiddle rucksack =
  let midpoint = length rucksack `div` 2
   in splitAt midpoint rucksack

-- find overlapping characters of two strings
findOverlap :: (String, String) -> String
findOverlap (rucksackA, rucksackB) = rucksackA `intersect` rucksackB

partOne input_string =
  let rucksacks = map splitMiddle $ lines input_string
      value = getValue . head . findOverlap
   in foldl (\acc x -> acc + value x) 0 rucksacks

-- Ha! Recursive functions :) find overlaps of a set of three strings
-- findOverlaps2 :: [String] -> [Char]
findBadges :: [String] -> [String]
findBadges (x : y : z : xs)
  | null xs = [findGroupBadge]
  | otherwise = findGroupBadge : findBadges xs
  where
    findGroupBadge = nub (findOverlap (x, y) `intersect` findOverlap (y, z))

-- partTwo :: String -> Int
partTwo input_string = sum . map (getValue . head) $ findBadges $ lines input_string
