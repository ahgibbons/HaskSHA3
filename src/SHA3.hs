module SHA3 where

import qualified Data.Array.Repa as R
import Data.Array.Repa hiding (map,zip,foldr)

type StateString = Array U DIM1 Bool
type StateArray  = Array U DIM3 Bool

log2Int :: Int -> Int
log2Int d  = log2Int' d 0
  where
    log2Int' 0 t = t
    log2Int' d t = log2Int' (d `div` 2) (t+1)

stringToArray :: StateString -> Int -> StateArray
stringToArray sstring w = computeS $ reshape (Z:.5:.5:.w) sstring

arrayToString :: StateArray -> Int -> StateString
arrayToString sarray w = computeS $ reshape (Z :. 5*5*w) sarray 


testString :: Array U DIM1 Int
testString = fromListUnboxed (Z :. 1600) [0..1599]

testArray :: Array U DIM3 Int
testArray = computeS $ reshape (Z :. 5 :. 5 :. 64) testString


sanityCheckStringArray :: Int -> Int -> Int -> Int -> Bool
sanityCheckStringArray x y z w = (testArray ! i1) == (testString ! i2)
  where
    i1 = (Z :. x :. y :. z)
    i2 = (Z :. w*(5*x + y) + z)

sanityCheckLane :: Int -> Int -> Int -> Bool
sanityCheckLane w i j = (toList $ lane testArray i j) == builtLane
  where builtLane = map (\z -> testArray .!. (i,j,z)) [0..(w-1)]

lane :: Array U DIM3 Int -> Int -> Int -> Array U DIM1 Int
lane sarray i j = computeS $ slice sarray (Any :. j :. i :. All)

plane :: Array U DIM3 Int -> Int -> Array U DIM1 Int
plane sarray j = computeS $ reshape (Z:.5*w) plane'
  where
    plane' = slice sarray (Any :. j :. All :. All)
    (Z:._:.w) = extent plane'

(.!.) :: Array U DIM3 Int -> (Int,Int,Int) -> Int
arr .!. (x,y,z) = arr ! (Z:.y:.x:.z)

theta :: Int -> StateArray -> StateArray
