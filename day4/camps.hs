import Data.List
import Data.List.Split
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStrLn $ "Part one is: " ++ show (partOne contents)
  putStrLn $ "Part two is: " ++ show (partTwo contents)

partOne :: String -> Int
partOne = solvePart $ folder (\a b -> a `isInfixOf` b || b `isInfixOf` a)

partTwo :: String -> Int
partTwo = solvePart $ folder (\a b -> not $ null $ a `intersect` b)

solvePart :: (Int -> [[Int]] -> Int) -> String -> Int
solvePart folder = foldl folder 0 . importAssignments

-- | Defines accumulator fn for fold
folder :: ([Int] -> [Int] -> Bool) -> (Int -> [[Int]] -> Int)
folder condition acc [a, b] = if condition a b then acc + 1 else acc

-- | Makes an overly nested list of pairs of ranges
importAssignments :: String -> [[[Int]]]
importAssignments contents =
  let assignPairs = map (splitOn ",") $ lines contents
   in map (map stringToRange) assignPairs

-- | Converts range represented as "a-b" to [a..b] form
stringToRange :: String -> [Int]
stringToRange my_string =
  let splitStr = splitOn "-" my_string
      rangeEnds = map (read :: String -> Int) splitStr
      makeRange [a, b] = [a .. b]
   in makeRange rangeEnds
