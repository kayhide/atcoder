import Data.List

main :: IO ()
main = do
  line <- getLine
  print $ length $ nub $ words line
