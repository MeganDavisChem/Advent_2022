import Data.List
import Data.List.Split
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStrLn $ "Part one is: " ++ show (solvePart p1Folder contents)
  putStrLn $ "Part two is: " ++ show (solvePart p2Folder contents)

solvePart :: (Int -> [[Int]] -> Int) -> String -> Int
solvePart folder = foldl folder 0 . importAssignments

p2Folder :: Int -> [[Int]] -> Int
p2Folder acc [a, b]
  | not (null (a `intersect` b)) = acc + 1
  | otherwise = acc

p1Folder :: Int -> [[Int]] -> Int
p1Folder acc [a, b]
  | a `isInfixOf` b || b `isInfixOf` a = acc + 1
  | otherwise = acc

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
