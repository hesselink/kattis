import Data.List (sortBy)
import Data.List.Split (splitOn, chunksOf)

main :: IO ()
main = interact $ \str ->
  let ls = drop 1 (lines str)
      cases = parseCases ls
      solutions = map solve cases
  in unlines (map printSolution solutions)

data Case = Case
  { number :: Int
  , numKeys :: Int
  , frequencies :: [Int]
  } deriving Show

parseCases :: [String] -> [Case]
parseCases = go 1
  where
    go _ [] = []
    go n (l1:l2:ls) =
      let keys = read (splitOn " " l1 !! 1)
          freqs = map read (splitOn " " l2)
      in Case n keys freqs : go (n+1) ls

data Solution = Solution
  { caseNumber :: Int
  , keyPresses :: Int
  } deriving Show

solve :: Case -> Solution
solve c =
  let sorted = sortBy (flip compare) (frequencies c)
      grouped = chunksOf (numKeys c) sorted
      total = sum . concat . zipWith (\i g -> map (*i) g) [1..] $ grouped
  in Solution (number c) total

printSolution :: Solution -> String
printSolution sol = "Case #" ++ show (caseNumber sol) ++ ": " ++ show (keyPresses sol)

