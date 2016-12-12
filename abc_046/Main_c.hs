import Data.List
import Control.Monad

next :: (Int, Int) -> (Int, Int) -> (Int, Int)
next (t, a) (t', a') = (t' * r, a' * r)
  where t'' = (t + t' - 1) `div` t'
        a'' = (a + a' - 1) `div` a'
        r = max t'' a''

readLine :: String -> (Int, Int)
readLine line = (x, y)
  where [x, y] = fmap read $ words line

main :: IO ()
main = do
  n <- read <$> getLine
  ratios <- fmap readLine <$> replicateM n getLine
  print $ uncurry (+) $ foldl' next (1, 1) ratios
