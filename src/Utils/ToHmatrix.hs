module Utils.ToHmatrix where 

import Data.Massiv.Array as M
import Numeric.LinearAlgebra as H

massivToArray :: M.Array M.U M.Ix2 Double -> [[Double]]
massivToArray arr = M.toLists2 arr

createHMatrix :: [[Double]] -> H.Matrix Double
createHMatrix = H.fromLists

convertMassivToHMatrix :: M.Array M.U M.Ix2 Double -> H.Matrix Double
convertMassivToHMatrix = createHMatrix . massivToArray

main :: IO ()
main = do
    -- Example: Create a 2D massiv array
    let massivArr = M.makeArrayR U M.Seq (M.Sz2 3 3) (\(i :. j) -> fromIntegral (i * j))
    -- Convert to hmatrix
    let hmatrixArr = convertMassivToHMatrix massivArr
    -- Use hmatrixArr in further computations
    print hmatrixArr
