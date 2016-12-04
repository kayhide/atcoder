import Control.Monad

isAcceptable :: Int -> Int -> Int -> Bool
isAcceptable a b c =
  (a == b + c) || (a + b == c) || (a + c == b)

main :: IO ()
main = do
  line <- getLine
  let [a, b, c] = read <$> words line :: [Int]
  putStrLn $ displayBool $ isAcceptable a b c
  where displayBool True = "Yes"
        displayBool False = "No"
