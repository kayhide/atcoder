import Control.Monad

terms :: String -> [[Int]]
terms [] = [[]]
terms nums = do
  i <- [1..(length nums)]
  let (x, xs) = splitAt i nums
  (read x:) <$> terms xs

main :: IO ()
main = do
  nums <- getLine
  print $ sum $ sum <$> terms nums
