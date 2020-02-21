import qualified Data.Map as Map
import Control.Monad
import Data.List
import Data.Maybe
import Test.QuickCheck
import Debug.Trace

main :: IO ()
main = interact $ unlines . map winner . tail . lines

winner :: String -> String
winner str = if wins str then "Chikapu" else "Bash"

wins :: String -> Bool
wins str | length str < 3 = False
         | smallestRepeatDistance str == Nothing = length str `mod` 2 == 1
         | smallestRepeatDistance str == Just (length str - 1) = length str `mod` 2 == 0
         | not (canDelete str) = False
         | otherwise = fromJust (smallestRepeatDistance str) `mod` 2 == length str `mod` 2
         -- | smallestRepeatDistance str == Just 2 = length str > 3 && length str `mod` 2 == 1
         -- | smallestRepeatDistance str == Just 2 = length str `mod` 2 == 1
         -- | otherwise =
         --     let matches = fromMaybe 1 (smallestRepeatDistance str) `mod` 2 == length str `mod` 2
         --     in if fromMaybe 1 (smallestRepeatDistance str) + 1 == length str
         --        then not matches else matches

slowWins str | length str < 3 = False
             | otherwise = or [ not (slowWins str') | str' <- deleteOne str ]

canDelete = any isValid . deleteOne

deleteOne :: String -> [String]
deleteOne (x:y:z:rest) | x /= z = (x:z:rest) : map (x:) (deleteOne (y:z:rest))
                       | otherwise = map (x:) (deleteOne (y:z:rest))
deleteOne _ = []

smallestRepeatDistance :: Ord a => [a] -> Maybe Int
smallestRepeatDistance = go Map.empty Nothing . zip [0..]
  where
    go _ res [] = res
    go seen res ((i,x):xs) = case Map.lookup x seen of
      Just prev | isNothing res || (i - prev) < fromJust res -> go (Map.insert x i seen) (Just (i - prev)) xs
      _ -> go (Map.insert x i seen) res xs

newtype Input = Input String
  deriving Show

instance Arbitrary Input where
  arbitrary = sized $ \n -> Input <$> replicateM (max 5 n) (choose ('a', 'z'))

isValid :: String -> Bool
isValid str = length (group str) == length str

test :: Input -> Property
test (Input str) = isValid str ==> traceShow (length str `mod` 2, fmap (`mod` 2) (smallestRepeatDistance str), wins str, str) $ wins str === slowWins str
