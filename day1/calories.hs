import Data.List
import System.IO

-- Open our file, pipe input into our functions and print results to stdout
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStrLn $ "The elf with the most calories has " ++ findMaxCal contents ++ " calories"
  putStrLn $ "Top three elves combined: " ++ findTopThree contents ++ " calories"

-- Just gets the max of output of formScannedList
findMaxCal :: String -> String
findMaxCal calstr = show . maximum $ formScannedList calstr

-- Scan the scanned list from the right and form new list where we blank out each value that isn't just before a
-- 0, i.e. only keep totals
-- Then do a bunch of mumbo jumbo to get the top three of the remaining numbers
findTopThree :: String -> String
findTopThree calstr = show . sum . take 3 . reverse . sort $ scanr1 mySorter $ formScannedList calstr
  where
    mySorter x acc
      | x == 0 || acc == 0 = x
      | otherwise = 1

-- Take raw string input, separate by line, convert each line to an integer, replacing blank lines with 0
-- Then, form a new list with a scan, where each element is the sum of the previous elements, with the
-- sum getting reset at each 0
formScannedList :: String -> [Integer]
formScannedList = scanCalIntList . convCalListToInts . lines
  where
    convCalListToInts = map calStrToCalInt
      where
        calStrToCalInt x
          | x == "" = 0
          | otherwise = read x :: Integer
    scanCalIntList = scanl1 (\acc x -> if x == 0 then 0 else acc + x)
