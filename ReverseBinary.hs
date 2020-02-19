import Numeric
import Data.Char

main :: IO ()
main = interact $ \input ->
  show . fst . head . readInt 2 (`elem` "01") (read . pure) . reverse . showIntAtBase 2 intToDigit (read input) $ ""
