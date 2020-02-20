{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Arrow (first, second)
import Control.Applicative (liftA, liftA2)
import Control.Monad (liftM, ap)
import Data.Function (on)
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Test.QuickCheck
import Debug.Trace

{-
main :: IO ()
main = quickCheck testFindMax
-}

main :: IO ()
main = interact $ \str ->
  let input :: [Int]
      input = sortBy (flip compare) . map read . words . head . drop 1 . lines $ str
  in show (ev input)

ev :: [Int] -> Double
ev input = maximum [ score submitted  | submitted <- heads input ]

-- Using this instead of maximum gives 'wrong answer'. Why?

findMax :: [Double] -> Double
findMax [] = 0
findMax [x] = x
findMax (x:y:xs) | y > x = findMax (y:xs)
                 | otherwise = x

score :: [Int] -> Double
score submitted = expectedValue $ rpi (inputToP submitted) (length submitted)

rpi :: (Functor f, Num (f Int)) => [f Int] -> Int -> f Double
rpi a s = fmap (rpi1 s) (sum a)

rpi1 :: Int -> Int -> Double
rpi1 s v = if v == 0
         then 0
         else
           let v' = fromIntegral v
           in v' ** (v' / fromIntegral s)

data P a where
  Prim :: Ord a => [(a, Double)] -> P a
  Pure :: a -> P a
  Bind :: P a -> (a -> P b) -> P b

instance (Ord a, Show a) => Show (P a) where
  show p = show (runP p)

instance Functor P where
  fmap = liftM -- mkP . fmap (first f) . unP

instance Applicative P where
  pure = Pure -- P [(n, 1)]
  (<*>) = ap -- mkP [ (a b, pa * pb) | (a, pa) <- as, (b, pb) <- bs ]

instance Monad P where
  (>>=) = Bind

{-
  join_ $ fmap f p
    where
      join_ (P ps) = P (concatMap join1 ps)
      join1 (P as, n) = map (second (* n)) as
      -}

data P2 a = P2 { unP2 :: [(a, Double)] }
  deriving Show

instance Functor P2 where
  fmap f = P2 . fmap (first f) . unP2

instance Applicative P2 where
  pure n = P2 [(n, 1)]
  P2 as <*> P2 bs = P2 [ (a b, pa * pb) | (a, pa) <- as, (b, pb) <- bs ]

instance Monad P2 where
  p >>= f = join_ $ fmap f p
    where
      join_ (P2 ps) = P2 (concatMap join1 ps)
      join1 (P2 as, n) = map (second (* n)) as

runP :: Ord a => P a -> [(a, Double)]
runP (Prim ps) = ps
runP (Pure v) = [(v, 1)]
runP (Bind (Prim ps) f) = simplify $ concat [ map (second (* p)) (runP (f v)) | (v, p) <- ps ]
runP (Bind (Pure a) f) = runP (f a)
runP (Bind (Bind pa f) g) = runP (Bind pa (\a -> Bind (f a) g))

simplify :: Ord a => [(a, Double)] -> [(a, Double)]
simplify = map (\g -> (fst . head $ g, sum . map snd $ g)) . groupBy ((==) `on` fst) . sortBy (comparing fst)

expectedValue :: Real a => P a -> Double
expectedValue = expectedValue' . runP

expectedValue2 :: Real a => P2 a -> Double
expectedValue2 = expectedValue' . unP2

expectedValue' :: Real a => [(a, Double)] -> Double
expectedValue' = foldl (\a (b, n) -> a + realToFrac b * n) 0

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
    inputToP1 n = Prim [(1, fromIntegral n / 100), (0, 1 - fromIntegral n / 100)]

inputToP2 :: [Int] -> [P2 Int]
inputToP2 = map pToP2 . inputToP

heads :: [a] -> [[a]]
heads xs = [ take n xs | let l = length xs, n <- [l, l-1 .. 1] ]

-- Testing

newtype Input = Input [Percentage]
  deriving Show

newtype Percentage = Percentage { getPercentage :: Int }
  deriving Show

instance Arbitrary Input where
  arbitrary = sized $ \n -> do
    l <- choose (1, n `max` 100)
    Input <$> vector l

instance Arbitrary Percentage where
  arbitrary = sized $ \n ->
    Percentage <$> choose (1, n `max` 100)

testFindMax :: Input -> Property
testFindMax (Input ps) =
  let input = sortBy (flip compare) . map getPercentage $ ps
      scored = traceShow input $ [ score submitted  | submitted <- heads input ]
  in maximum scored === findMax scored

testPisP2 :: P Int -> Property
testPisP2 p =
  let p2 = pToP2 p
      lt :: (Ord a, Show a) => a -> a -> Property
      x `lt` y = counterexample (show x ++ interpret res ++ show y) res
        where
          res = x < y
          interpret True = " < "
          interpret False = " >= "
  in abs (expectedValue p - expectedValue2 p2) `lt` (10**(-6))

pToP2 :: P a -> P2 a
pToP2 (Prim ps) = P2 ps
pToP2 (Pure v) = pure v
pToP2 (Bind p f) = pToP2 p >>= pToP2 . f

instance (Ord a, Arbitrary a, CoArbitrary a) => Arbitrary (P a) where
  arbitrary = sized arbP
    where
      arbP 0 = oneof [ Prim <$> arbitrary
                     , Pure <$> arbitrary
                     ]
      arbP n = oneof [ Prim <$> arbitrary
                     , Pure <$> arbitrary
                     , resize (n `div` 2) $ Bind <$> (arbitrary :: Gen (P a)) <*> arbitrary
                     ]
