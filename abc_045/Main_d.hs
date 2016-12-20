{-# LANGUAGE TupleSections #-}
import Control.Monad
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.ByteString.Char8 as C

type View = (Int, Int)
type Spot = (Int, Int)

viewsFor :: (Int, Int) -> [Spot] -> [View]
viewsFor (w, h) spots = do
  (x, y) <- spots
  let is = [(max 1 (x - 2)) .. (min (w - 2) x)]
      js = [(max 1 (y - 2)) .. (min (h - 2) y)]
  (,) <$> is <*> js

readLine :: IO Spot
readLine = do
  [Just (y, _), Just(x, _)] <- (fmap C.readInt . C.words) <$> C.getLine
  return (x, y)

main :: IO ()
main = do
  [h, w, n] <- (fmap read . words) <$> getLine
  spots <- replicateM n $ readLine
  let spotCounts = Map.fromListWith (+) $ (,1) <$> viewsFor (w, h) spots
      viewCounts = IntMap.fromListWith (+) $ (,1) <$> Map.elems spotCounts
      zero = (w - 2) * (h - 2) - sum (IntMap.elems viewCounts)
      nums = flip (IntMap.findWithDefault 0) viewCounts <$> [1..9]
  mapM_ print (zero:nums)
