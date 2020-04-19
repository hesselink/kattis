import Data.Bits
import Data.List

main :: IO ()
main = interact $ \str ->
  let numbers = map read . init . lines $ str
  in unlines . map threePowers $ numbers

threePowers :: Integer -> String
threePowers n = (\s -> if null s then "{ }" else "{ " ++ s ++ " }") . intercalate ", " . map (show . (3^)) . setBitIndices $ n - 1

setBitIndices :: Integer -> [Int]
setBitIndices n = filter (testBit n) [0..63] -- max 19 digits
