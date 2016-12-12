import Data.List

main :: IO ()
main = do
  line <- getLine
  let gs = length $ filter (=='g') line
      ps = length line - gs
  print $ (gs - ps) `div` 2
