{-# LANGUAGE BangPatterns #-}

module Local_spaces.KNN where 
    
import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Utils.DataIn

-- import Data.Massiv.Array as M


-- testing 
import Data.Massiv.Array as M
import Distribution.Fields (SectionArg(SecArgName))
-- import Data.Time.Format.ISO8601 (yearFormat)

type DataPoint = M.Array M.U M.Ix1 Double -- A data point is a 1D Unboxed array
type MatrixT = M.Array M.U M.Ix2 Double    -- Matrix is a 2D Unboxed array

-- Euclidean distance function
euclideanDistance :: M.Array M.U M.Ix1 Double -> M.Array M.U M.Ix1 Double -> Double
euclideanDistance p1 p2 = sqrt . M.sum $ M.zipWith (\x y -> (x - y) ** 2) (p1) (p2)

-- Compute a distance row
distanceRow :: MatrixT -> Int -> Array U Ix1 Double
distanceRow matrix rowIndex = 
  let row = matrix !> rowIndex
      n = getAmt (size matrix)
  in makeArray Seq (Sz (n)) (\i -> euclideanDistance row (matrix !> i))

getDim :: Sz Ix2 -> Int 
getDim sz = let (m :. n) = unSz sz in n

getAmt :: Sz Ix2 -> Int 
getAmt sz = let (m :. n) = unSz sz in m 


-- Compute the distance matrix
distanceMatrix :: MatrixT -> M.Array M.D M.Ix1 (M.Array M.U M.Ix1 Double)
--  Compute the distance matrix D
-- Implement boring brute force to illustrate idea
-- If time permits, implement nearest neighbor descent algo

distanceMatrix matrix =
  let n = getAmt (size matrix)
      rows = M.map (distanceRow matrix) (makeVectorR D Seq (Sz (n)) id)
      in rows 
      -- Convert each row from 1D to 2D array
  --     matrixRows = M.map (\row -> resize' (Sz (1 :. (n+1))) row) rows
  -- in computeAs U $ M.concat' 2 matrixRows

-- -- distanceMatrix :: MatrixT -> MatrixT 
-- distanceMatrix matrix =
--   let n = getAmt (size matrix)
--       rows = Prelude.map (distanceRow matrix) [0 .. (n)] 
--       -- Convert each row from 1D to 2D array
--       matrixRows = Prelude.map (\row -> resize' (Sz (1 :. (n+1))) row) rows
--   in computeAs U $ M.concat' 2 matrixRows  -- Concatenate along the first dimension


-- Example usage
dataPoints :: MatrixT
dataPoints = fromLists' Seq [[1,2], [3,4], [5,6]]


resultMatrix = distanceMatrix dataPoints

nearestNeighbors :: Int -> Array U Ix1 Double -> Array B Ix1 Indexed
nearestNeighbors k distances1 = 
  let distances = computeAs B distances1
      n = length distances
      indices = makeVectorR B Seq (fromIntegral n) id -- Note: n - 1 because of zero-based indexing
      together = addIndexes indices distances
      closestK = extractFirstK k $ M.quicksort together
  in closestK

          -- k_closest = extractFirstK k (M.quicksort (fromList together))
          -- in k_closest

extractFirstK :: Int -> Array B Ix1 Indexed -> Array B Ix1 Indexed
extractFirstK k arr = computeAs B $ extract' 1 (Sz1 k) arr


kNearestNeighbors :: Int -> M.Array M.D M.Ix1 (M.Array M.U M.Ix1 Double) -> M.Array M.D M.Ix1 (M.Array M.B M.Ix1 Indexed)
-- For each row in D, find indices of k smallest values
-- For each row, map distances to indices. 
-- Sort by distance, and then create two new vectors of indices and distances. 
-- Put vector into matrix and move into next row. 
kNearestNeighbors k allDistances = 
    let n = length allDistances
        knn_closestNindices = M.map (nearestNeighbors k) allDistances
        in knn_closestNindices

-- Implement the smooth approximator
-- row and sigma 
-- row_i =min{d(xi,xij)|1≤j≤k,d(xi,xij)>0},



addIndexes :: M.Array M.B M.Ix1 Int -> M.Array M.B M.Ix1 Double -> M.Array M.B M.Ix1 Indexed
addIndexes indices distances = computeAs B $ M.zipWith Indexed indices distances
  

data Indexed = Indexed Int Double 
  deriving Show

at :: Double -> Int -> Indexed
a `at` i = Indexed i a -- **FILL IN HERE**

item :: Indexed -> Double
item (Indexed i a) = a

index :: Indexed -> Int
index (Indexed i a) = i

instance Eq Indexed where 
  -- (==) :: Eq i => Indexed i a -> Indexed i a -> Bool
  x == y = Local_spaces.KNN.index x == Local_spaces.KNN.index y

instance Ord Indexed where
  -- compare :: Ord i => Indexed i a -> Indexed i a -> Ordering
  compare x y 
    | Local_spaces.KNN.item x < Local_spaces.KNN.item y = LT
    | Local_spaces.KNN.item x > Local_spaces.KNN.item y = GT
    | otherwise = EQ

-- addIndexes :: [Int] -> [Double] -> [Indexed]
-- addIndexes (ix:ixs) (a:as) = [Indexed ix a] ++ addIndexes ixs as
-- addIndexes _ [] = []
-- addIndexes [] _ = []


removeIndexes :: [Indexed] -> [Double]
removeIndexes (x:xs) = [item x] ++ removeIndexes xs
removeIndexes [] = []

removeItems :: [Indexed] -> [Int]
removeItems (x:xs) = [Local_spaces.KNN.index x] ++ removeItems xs 
removeItems [] = []








-- localSpaces :: M.Array M.U M.Ix2 Float -> M.Array M.U M.Ix2 Float -> M.Array M.U M.Ix2 Float
-- localSpaces m knn = undefined-- For each row in M, use KNN to create local space submatrices
-- -- calculate row and sigma 
-- -- Set all of the rows in amt lengthed vecotr to zero
-- -- Go through knn and compute fuzzy score
-- -- add to the next 

-- fuzzyComplex :: M.Array M.U M.Ix2 Float -> Int -> M.Array M.U M.Ix2 Float
-- fuzzyComplex lsm amt = undefined-- LS are the local spaces
-- -- The goal here is to have a symmetric matrix that is a combination of all the local spaces. 
--   -- map over all 

-- -- instance Functor DataPoints where 
-- --   fmap :: DataPoints


sortIndexedVector :: Array D Ix1 Indexed -> Array B Ix1 Indexed
sortIndexedVector arr = M.quicksort $ computeAs B arr


main =
    let arr = makeVectorR D Seq 10 (\i -> let x = 0.5 ** fromIntegral i in Indexed i x)
        sortedArr = sortIndexedVector arr
    in sortedArr