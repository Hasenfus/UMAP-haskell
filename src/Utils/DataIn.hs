module Utils.DataIn where 

{-# LANGUAGE OverloadedStrings #-}

import Data.Massiv.Array as M
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

type Row = [Double] -- Define Row according to your CSV structure

-- Function to convert a Vector of Rows to a Massiv Array
vectorToMassivArray :: V.Vector Row -> M.Array M.U M.Ix2 Double
vectorToMassivArray vec = M.resize' (M.Sz2 (V.length vec) (length $ V.head vec)) $ M.fromList M.Seq (concat $ V.toList vec)


-- data DataPoints = DataPoints 
--     {
--         dataArray :: M.Array M.U M.Ix2 Float, 
--         amt :: Int,
--         dim :: Int 
--     }

-- createDataSet :: Int -> Int -> M.Array M.U M.Ix2 Float -> DataSet
-- createDataSet rows cols info = DataSet
--   { dataArray = info,
--     int1 = i1,
--     int2 = i2
--   }

-- accessElement :: DataSet -> (Int, Int) -> Double
-- accessElement ds (i, j) = dataArray ds ! (i :. j)


csv2matrix :: String -> IO (Maybe (M.Array M.U M.Ix2 Double))
csv2matrix file = do 
    csvData <- BL.readFile ("src/Data/" ++ file)
    case decode NoHeader csvData of
        Left err -> return Nothing
        Right v -> do
            let arrayData = vectorToMassivArray v
            -- Now arrayData is a Massiv Array with your CSV data
            return (Just arrayData)



        