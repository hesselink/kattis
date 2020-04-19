import Data.Bits

main :: IO ()
main = interact $ \str ->
  let numbers = tail $ lines str
  in unlines . map (show . maxPopCount . read) $ numbers

maxPopCount :: Int -> Int
maxPopCount = maximum . map popCount . intermediateNumbers

intermediateNumbers :: Int -> [Int]
intermediateNumbers n =
  let s = reverse (show n)
  in map (read . reverse) . takeWhile (not . null) . iterate (drop 1) $ s
