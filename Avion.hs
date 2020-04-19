import Data.List

main :: IO ()
main = interact $ \str ->
  let registrations = lines str
      matchingIndices = map fst . filter (("FBI" `isInfixOf`) . snd) . zip [1::Int ..] $ registrations
      output = if null matchingIndices
               then "HE GOT AWAY!"
               else unwords (map show matchingIndices)
  in output
