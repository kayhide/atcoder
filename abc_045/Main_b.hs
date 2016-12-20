play :: (String, String, String) -> Char -> String
play ([], _, _) 'a' = "A"
play (_, [], _) 'b' = "B"
play (_, _, []) 'c' = "C"
play ((x:xs), ys, zs) 'a' = play (xs, ys, zs) x
play (xs, (y:ys), zs) 'b' = play (xs, ys, zs) y
play (xs, ys, (z:zs)) 'c' = play (xs, ys, zs) z

main :: IO ()
main = do
  (x:xs) <- getLine
  ys <- getLine
  zs <- getLine
  putStrLn $ play (xs, ys, zs) x
