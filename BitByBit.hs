import Data.List.Split (splitOn)

main :: IO ()
main = interact $ \str ->
  let inputs = init (parseInputs (lines str))
      results = map (calculateResult emptyBits) inputs
  in unlines (map printResult results)

type Input = [Instr]
data Instr
  = Clear Int
  | Set Int
  | Or Int Int
  | And Int Int

parseInputs :: [String] -> [Input]
parseInputs [] = []
parseInputs (l:ls) =
  let numInstrs = read l
      (instrLines, restLines) = splitAt numInstrs ls
      instrs = map parseInstr instrLines
  in instrs : parseInputs restLines

parseInstr :: String -> Instr
parseInstr l =
  let (op:args) = splitOn " " l
  in case op of
       "CLEAR" -> Clear (read (head args))
       "SET" -> Set (read (head args))
       "OR" -> Or (read (args !! 0)) (read (args !! 1))
       "AND" -> And (read (args !! 0)) (read (args !! 1))

type Bits = [Bit]

data Bit
  = Unknown
  | Zero
  | One
  deriving (Eq, Show)

emptyBits :: Bits
emptyBits = replicate 32 Unknown

calculateResult :: Bits -> Input -> Bits
calculateResult = foldl runInstr

runInstr :: Bits -> Instr -> Bits
runInstr bs instr = case instr of
  Clear i -> setAt i Zero bs
  Set i -> setAt i One bs
  Or i j -> setAt i (orBit (bs !! i) (bs !! j)) bs
  And i j ->  setAt i (andBit (bs !! i) (bs !! j)) bs

orBit :: Bit -> Bit -> Bit
orBit One _ = One
orBit _ One = One
orBit Unknown _ = Unknown
orBit _ Unknown = Unknown
orBit _ _ = Zero

andBit :: Bit -> Bit -> Bit
andBit Zero _ = Zero
andBit _ Zero = Zero
andBit Unknown _ = Unknown
andBit _ Unknown = Unknown
andBit _ _ = One

printResult :: Bits -> String
printResult = reverse . map printBit

printBit :: Bit -> Char
printBit Unknown = '?'
printBit Zero = '0'
printBit One = '1'

setAt :: Int -> a -> [a] -> [a]
setAt 0 x (_:xs) = x:xs
setAt _ _ [] = error "out of bounds"
setAt n x (x':xs) = x' : setAt (n-1) x xs
