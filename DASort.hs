import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = interact mainStr

mainStr :: String -> String
mainStr str =
  let datasets = parseInput str
      solutions = map solve datasets
  in unlines (map printSolution solutions)

data Dataset = Dataset
  { number :: Int
  , data_ :: [Int]
  } deriving Show

parseInput :: String -> [Dataset]
parseInput inStr =
  let ls = drop 1 (lines inStr)
  in parseDatasets ls

parseDatasets :: [String] -> [Dataset]
parseDatasets [] = []
parseDatasets (l:ls) =
  let (nStr, dataLenStr) = break (== ' ') l
      dataLen = read dataLenStr
      numDataLines = ceiling (fromIntegral dataLen / 10)
      (dataLines, restLines) = splitAt numDataLines ls
      parsedData = concatMap (map read . splitOn " ") dataLines
  in Dataset (read nStr) parsedData : parseDatasets restLines

data Solution = Solution
  { datasetNumber :: Int
  , minOperations :: Int
  } deriving Show

solve :: Dataset -> Solution
solve ds =
  let sorted = sort (data_ ds)
      left = removeInOrder sorted (data_ ds)
  in Solution (number ds) (length left)

removeInOrder :: Eq a => [a] -> [a] -> [a]
removeInOrder [] _ = []
removeInOrder _ [] = []
removeInOrder (x:sorted) (y:dat) | x == y = removeInOrder sorted dat
                                 | otherwise = y : removeInOrder (x:sorted) dat

printSolution :: Solution -> String
printSolution solution = show (datasetNumber solution) ++ " " ++ show (minOperations solution)
