import Control.Arrow (first, second)
import Control.Applicative (liftA, liftA2)
import Data.List (sortBy)

main :: IO ()
main = interact $ \str ->
  let input :: [Int]
      input = sortBy (flip compare) . map read . words . head . drop 1 . lines $ str
  in show (ev input)

ev :: [Int] -> Double
ev input = maximum [ score submitted  | submitted <- heads input ]

{-

Using this instead of maximum gives 'wrong answer'. Why?

findMax :: [Double] -> Double
findMax [] = 0
findMax [x] = x
findMax (x:y:xs) | y > x = findMax (y:xs)
                 | otherwise = x
                 -}


score :: [Int] -> Double
score submitted = expectedValue $ rpi (inputToP submitted) (length submitted)

rpi :: [P Int] -> Int -> P Double
rpi a s = fmap (rpi1 s) (sum a)

rpi1 :: Int -> Int -> Double
rpi1 s v = if v == 0
         then 0
         else
           let v' = fromIntegral v
           in v' ** (v' / fromIntegral s)

newtype P a = P { unP :: [(a, Double)] }
  deriving Show

instance Functor P where
  fmap f = P . fmap (first f) . unP

instance Applicative P where
  pure n = P [(n, 1)]
  P as <*> P bs = P [ (a b, pa * pb) | (a, pa) <- as, (b, pb) <- bs ]

instance Monad P where
  p >>= f = join_ $ fmap f p
    where
      join_ (P ps) = P (concatMap join1 ps)
      join1 (P as, n) = map (second (* n)) as

expectedValue :: P Double -> Double
expectedValue = foldl (\a (b, n) -> a + b * n) 0 . unP

instance Num a => Num (P a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = liftA abs
  signum = liftA signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (P a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational

instance Floating a => Floating (P a) where
  pi = pure pi
  exp = liftA exp
  log = liftA log
  sin = liftA sin
  cos = liftA cos
  asin = liftA asin
  acos = liftA acos
  atan = liftA atan
  sinh = liftA sinh
  cosh = liftA cosh
  asinh = liftA asinh
  acosh = liftA acosh
  atanh = liftA atanh

inputToP :: [Int] -> [P Int]
inputToP = map inputToP1
  where
    inputToP1 n = P [(1, fromIntegral n / 100), (0, 1 - fromIntegral n / 100)]

heads :: [a] -> [[a]]
heads xs = [ take n xs | let l = length xs, n <- [l, l-1 .. 1] ]
