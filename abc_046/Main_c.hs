import Data.List
import Control.Monad

next :: (Int, Int) -> (Int, Int) -> (Int, Int)
next (t, a) (t', a') = (t' * r, a' * r)
  where t'' | t `mod` t' == 0 = t `div` t'
            | otherwise = t `div` t' + 1
        a'' | a `mod` a' == 0 = a `div` a'
            | otherwise = a `div` a' + 1
        r = max t'' a''

readLine :: String -> (Int, Int)
readLine line = (x, y)
  where [x, y] = fmap read $ words line

main :: IO ()
main = do
  n <- read <$> getLine
  ratios <- fmap readLine <$> replicateM n getLine
  print $ uncurry (+) $ foldl' next (1, 1) ratios
