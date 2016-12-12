import Data.List

main :: IO ()
main = do
  line <- getLine
  let [n, k] = fmap read $ words line :: [Int]
  print $ head $ drop (n - 1) $ iterate (* (k - 1)) k
