import Data.List
import Control.Monad

next :: (Int, Int) -> (Int, Int) -> (Int, Int)
next (t, a) (t', a') = (t' * r, a' * r)
  where t'' = ceiling $ fromIntegral t / fromIntegral t'
        a'' = ceiling $ fromIntegral a / fromIntegral a'
        r = max t'' a''

readLine :: String -> (Int, Int)
readLine line = (x, y)
  where [x, y] = fmap read $ words line

main :: IO ()
main = do
  x <- getLine
  let n = read x :: Int
  tas <- replicateM n getLine
  let rates = fmap readLine tas
  print $ uncurry (+) $ foldl' next (1, 1) rates
