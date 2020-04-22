import Data.Char
import Data.Set (Set, (\\))
import Data.List (intercalate)
import qualified Data.Set as Set

main = interact $ intercalate "\n" . map (output . missingLetters) . tail . lines
  where
    output "" = "pangram"
    output m  = "missing " ++ m

missingLetters :: String -> String
missingLetters str = Set.toList $ missingLetters' (normalize str)
  where
    missingLetters' nStr = allLetters \\ nStr

normalize :: String -> Set Char
normalize = Set.fromList . filter isAlpha . map toLower

allLetters = Set.fromList ['a'..'z']
