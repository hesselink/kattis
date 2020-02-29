main :: IO ()
main = interact $ \str ->
  let correct = map (read . pure) . head . tail . lines $ str
      score = numCorrect correct
      scores = [("Adrian", score adrian), ("Bruno", score bruno), ("Goran", score goran)]
      maxCorrect = maximum . map snd $ scores
      namesWithMax = map fst . filter ((== maxCorrect) . snd) $ scores
  in unlines (show maxCorrect : namesWithMax)

data Answer = A | B | C
  deriving (Show, Read, Eq)

adrian, bruno, goran :: [Answer]
adrian = cycle [A,B,C]
bruno = cycle [B,A,B,C]
goran = cycle [C,C,A,A,B,B]

numCorrect :: [Answer] -> [Answer] -> Integer
numCorrect correct guesses =  sum $ zipWith (\c g -> if c == g then 1 else 0) correct guesses
