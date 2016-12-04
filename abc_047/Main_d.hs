import Data.List
import Data.Function

withMinimum :: [Int] -> [(Int, Int)]
withMinimum [] = []
withMinimum xs = withMinimum' xs (head xs)

withMinimum' :: [Int] -> Int -> [(Int, Int)]
withMinimum' [] _ = []
withMinimum' (x:xs) m = (x, min') : withMinimum' xs min'
  where min' = min x m

maximumsCount :: [Int] -> Int
maximumsCount xs = fst res
  where res = foldl' next (0, 0) xs
        next (acc, x) y
          | y > x = (1, y)
          | y == x = (acc + 1, x)
          | otherwise = (acc, x)

main :: IO ()
main = do
  getLine
  line <- getLine
  let xs = read <$> words line :: [Int]
  print $ maximumsCount $ fmap (uncurry (-)) $ withMinimum xs
