main = interact $ \str ->
  let ns = map (read :: String -> Integer) . tail . lines $ str
  in unlines . map (show . length . show . abs) $ ns
