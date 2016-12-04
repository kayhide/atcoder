import Data.List

main :: IO ()
main = do
  line <- getLine
  print $ length $ tail $ group line
