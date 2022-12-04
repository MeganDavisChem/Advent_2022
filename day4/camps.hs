import Data.List
import Data.List.Split
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStrLn $ "Part one is: " ++ show (partOne contents)
  putStrLn $ "Part two is: " ++ show (partTwo contents)

-- Scan over each pair, add to accumulator if a is a subset of b
partOne :: String -> Int
partOne contents =
  let assignments = importAssignments contents
   in foldl myScanner 0 assignments
  where
    myScanner acc [a, b] =
      if a `isInfixOf` b || b `isInfixOf` a
        then acc + 1
        else acc

-- Scan over each pair, add to accumulator if a and b have any overlap
partTwo :: String -> Int
partTwo contents =
  let assignments = importAssignments contents
   in foldl myScanner 0 assignments
  where
    myScanner acc [a, b] =
      if not (null (a `intersect` b))
        then acc + 1
        else acc

-- Makes an overly nested list of pairs of ranges
importAssignments :: String -> [[[Int]]]
importAssignments contents =
  let assignPairs = map (splitOn ",") $ lines contents
   in map (map stringToRange) assignPairs

-- Converts range represented as "a-b" to [a..b] form
stringToRange :: String -> [Int]
stringToRange my_string =
  let splitStr = splitOn "-" my_string
      rangeEnds = [read x :: Int | x <- splitStr]
      makeRange [a, b] = [a .. b]
   in makeRange rangeEnds
