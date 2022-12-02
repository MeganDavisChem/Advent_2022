import Data.List
import System.IO

-- Open our file, pipe input into our functions and print results to stdout
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStrLn $ "The elf with the most calories has " ++ processList maximum contents ++ " calories"
  putStrLn $ "Top three elves combined: " ++ processList (sum . take 3) contents ++ " calories"

-- Sum blocks of numbers and apply arbitrary function to list of sums
processList :: ([Integer] -> Integer) -> String -> String
processList function = show . function . sumBlocks . formIntList

-- Parse list of integers separated by blank lines and convert to list separated by zeroes
formIntList :: String -> [Integer]
formIntList = map (\x -> if x == "" then 0 else read x :: Integer) . lines

-- Find sums of numbers in a list separated by 0s, return in descending order
sumBlocks :: [Integer] -> [Integer]
sumBlocks = reverse . sort . removeNonFinal . scanBlocks
  where
    -- Set all but final sums from scanBlocks to -1, then filter
    removeNonFinal =
      filter (> 0) . scanr1 (\x acc -> if x == 0 || acc == 0 then x else -1)
    -- Make a running total that resets with each 0
    scanBlocks = scanl1 (\acc x -> if x == 0 then 0 else acc + x)
