{-# LANGUAGE FlexibleContexts #-}


module SimplicialComplex.Fuzzy where 

import Local_spaces.KNN
import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Utils.DataIn

import Data.Massiv.Array as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV


-- testing 
import Data.Massiv.Array as M
-- There exists a functor FinSing
-- FinSing :: local metric spaces -> simplicial set
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V


import Control.Monad.ST (runST)


topRep :: M.Array M.D M.Ix1 (M.Array M.B M.Ix1 Indexed)-> Int -> M.Array M.U M.Ix2 Double
topRep distancesIndices amt = 
  let rows = M.map (knn2adjRow amt) distancesIndices
      matrixRows = M.map (\row -> resize' (Sz (1 :. (amt))) row) rows
  in computeAs U $ M.concat' 2 matrixRows



knn2adjRow :: Int -> Array B Ix1 Indexed -> Array B Ix1 Double
knn2adjRow n distInd = 
  let -- Convert massiv array to list, then to vector
      distIndVec = V.fromList $ M.toList distInd
      -- Create an initial vector filled with zeros
      initAdjVec = V.replicate n 0.0
  in -- Convert updated vector back to massiv array
     M.fromList Seq $ V.toList $ runST $ do
        mVec <- V.thaw initAdjVec  -- Make the vector mutable
        -- Perform updates
        V.forM_ distIndVec $ \(Indexed idx weight) ->
          MV.modify mVec (const weight) idx
        V.freeze mVec




knn2adjRow2 :: Int -> Array B Ix1 Indexed -> Array B Ix1 Double
knn2adjRow2 n distInd = 
  let initAdjRow = makeArray Seq (Sz1 n) (const 0.0) :: Array B Ix1 Double
      idxWeights = M.map (\(Indexed idx weight) -> (idx, weight)) distInd
  in overlayAdjRow initAdjRow idxWeights
  where
    overlayAdjRow :: Array B Ix1 Double -> Array D Ix1 (Int, Double) -> Array B Ix1 Double
    overlayAdjRow arr iw = computeAs M.B $ M.zipWith (\(i, w) x -> if i == M.unSz (size arr) then x else if i == M.unSz (M.size arr) then x else w) iw arr

