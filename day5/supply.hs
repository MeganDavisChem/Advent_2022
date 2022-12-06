import Data.Char
import Data.List
import Data.List.Split
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStrLn $ "Part one is: " ++ partOne contents
  putStrLn $ "Part two is: " ++ partTwo contents

partOne :: String -> String
partOne contents = "hello, world"

partTwo :: String -> String
partTwo contents = "hello world"

-- Make a list each stack, all in one giant list
-- So, String -> [[Char]]
-- parseStacks :: String -> [[String]]
--
-- getMoves :: [String] -> [[Int]]
-- getMoves parsedInputs =
--  let
--  justDigits puts = map (map (filter isDigit) . words) $ filter (isPrefixOf "move") parsedInputs
--  noBlanks = (map (filter (\x -> length x > 0))) . justDigits
--   in noBlanks
--  in map (map (read :: String -> Int))  noBlanks
--
--

-- moveStack :: [String] -> [Int] -> [String]
moveStack stacks [amnt, src, dst] =
  let sourceStack = stacks !! (src - 1)
      destStack = stacks !! (dst - 1)
      movedBlocks = take amnt sourceStack
      newSource = drop amnt sourceStack
      newDest = (reverse movedBlocks) ++ destStack
      newStack1 = replaceNth (src - 1) newSource stacks
   in replaceNth (dst - 1) newDest newStack1

moveAllStacks :: [String] -> [[Int]] -> [String]
moveAllStacks stacks (move : []) = moveStack stacks move
moveAllStacks stacks (move : moves) = moveAllStacks (moveStack stacks move) moves

getFinalStacks :: String -> [String]
getFinalStacks contents =
  let parsedInputs = parseInput contents
      stacks = getStacks parsedInputs
      moves = intMoves parsedInputs
   in moveAllStacks stacks moves

-- Shamelessly copied from stackexchange
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceNth (n - 1) newVal xs

onlyDigitMoves :: [String] -> [[String]]
onlyDigitMoves = map (map (filter isDigit) . words) . filter (isPrefixOf "move")

noBlankMoves :: [[String]] -> [[String]]
noBlankMoves = (map (filter (\x -> length x > 0)))

intMoves :: [String] -> [[Int]]
intMoves moves = map (map (read :: String -> Int)) $ noBlankMoves . onlyDigitMoves $ moves

parseInput contents =
  let inputLines = lines contents
      -- Absolutely disgusting
      whitespace = replicate 4 ' '
      splitLines = map (split $ dropDelims (onSublist "] [")) inputLines
      moreSplitLines = map (map (split $ onSublist whitespace)) splitLines
      cleanedLines = map (map (map (delete '[' . delete ']'))) moreSplitLines
      lessWhiteSpace = map (map (map (split (dropDelims $ onSublist "   ")))) cleanedLines
      -- cleanedABit = map (concat . map concat . (map concat)) lessWhiteSpace
      cleanedABit = map (concatMap (concat . concat)) lessWhiteSpace
   in -- TODO parse out moves separately
      --      splitLines = map (split (onSublist whitespace)) splitlines
      -- splitLines = map (splitOn whitespace) inputLines
      cleanedABit

getStacks :: [String] -> [String]
getStacks parsedInput =
  let noEmpty = filter (not . null) parsedInput
      noNum = filter (not . checkHasNum) parsedInput
   in map (dropWhile (not . isAlphaNum)) $ transpose $ init noNum

checkHasNum :: String -> Bool
checkHasNum = foldr (\y acc -> isDigit y || acc) False

-- Split this
