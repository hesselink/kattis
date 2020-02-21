main :: IO ()
main = interact $ \str ->
  let (pStr:_:aStr) = lines str
  in show $ play 210 (read pStr) (map parseAnswer aStr)

data Answer = Answer
  { seconds :: Seconds
  , result :: Result
  } deriving Show

type Seconds = Int
type Player = Int
data Result = Correct | Incorrect | Skipped
  deriving (Show, Eq)

parseAnswer :: String -> Answer
parseAnswer str =
  let (n:c:_) = words str
  in Answer (read n) (parseResult c)

parseResult :: String -> Result
parseResult str = case str of
  "T" -> Correct
  "N" -> Incorrect
  "P" -> Skipped
  _ -> error $ "Unknown result string: " ++ str

play :: Seconds -> Player -> [Answer] -> Player
play _ player [] = player
play secs player (answer:answers) =
  let (newSeconds, newPlayer) = turn answer secs player
  in if newSeconds > 0
     then play newSeconds newPlayer answers
     else player

turn :: Answer -> Seconds -> Player -> (Seconds, Player)
turn answer timeLeft curPlayer =
  let newTimeLeft = timeLeft - seconds answer
      passed = newTimeLeft > 0 && result answer == Correct
      newPlayer = if passed then curPlayer `mod` 8 + 1 else curPlayer
  in (newTimeLeft, newPlayer)
