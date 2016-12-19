import Control.Monad

main :: IO ()
main = do
  [a, b, h] <- replicateM 3 $ read <$> getLine
  print $ (a + b) * h `div` 2
