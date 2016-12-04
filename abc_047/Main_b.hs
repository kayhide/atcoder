import Control.Monad

type Point = (Int, Int)
type Square = (Point, Point)
data Direction = West | East | North | South deriving (Show, Eq, Ord, Enum)

fill :: Square -> (Point, Direction) -> Square
fill ((x1, y1), (x2, y2)) ((x, y), West) = ((max x1 x, y1), (x2, y2))
fill ((x1, y1), (x2, y2)) ((x, y), East) = ((x1, y1), (min x2 x, y2))
fill ((x1, y1), (x2, y2)) ((x, y), North) = ((x1, max y1 y), (x2, y2))
fill ((x1, y1), (x2, y2)) ((x, y), South) = ((x1, y1), (x2, min y2 y))

readLine :: String -> (Point, Direction)
readLine str = ((x, y), dir)
  where [x, y, i] = read <$> words str :: [Int]
        dir = toEnum (i - 1) :: Direction

area :: Square -> Int
area ((x1, y1), (x2, y2)) = (max 0 (x2 - x1)) * (max 0 (y2 - y1))

main :: IO ()
main = do
  line <- getLine
  let [w, h, n] = read <$> words line :: [Int]
  lines <- replicateM n getLine
  let res = foldl fill ((0, 0), (w, h)) $ fmap readLine lines
  print $ area res
