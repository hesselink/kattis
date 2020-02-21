import Data.List

main = interact $ \str ->
  let r = read str
  in intercalate "\n" . map show $ [circleArea r, taxicabArea r]

circleArea :: Int -> Double
circleArea r = pi * (fromIntegral r ** 2)

taxicabArea :: Int -> Double
taxicabArea r =
  let halfSide = (fromIntegral r ** 2) / 2
  in halfSide * 4
