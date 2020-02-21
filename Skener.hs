main :: IO ()
main = interact $ \str ->
  let (l1:ls) = lines str
      [_,_,zr,zc] = map read . words $ l1
  in unlines . concatMap (replicate zr) . map (concatMap (replicate zc)) $ ls
