import Control.Monad

terms :: String -> [[Int]]
terms [] = [[]]
terms nums = do
  i <- [1..(length nums)]
  let x = read $ take i nums :: Int
  (x:) <$> terms (drop i nums)

main :: IO ()
main = do
  nums <- getLine
  print $ sum $ sum <$> terms nums
