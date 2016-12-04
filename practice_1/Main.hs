main :: IO ()
main = do
  l1 <- getLine
  l2 <- getLine
  l3 <- getLine
  let a = read l1 :: Int
      [b, c] = fmap read $ words l2 :: [Int]
  putStrLn $ show (a + b + c) ++ " " ++ l3
