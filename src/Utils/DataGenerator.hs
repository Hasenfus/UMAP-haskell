module Utils.DataGenerator where 

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.List (intercalate)


generateRandomRow :: Int -> IO [Double]
generateRandomRow m = replicateM m (randomRIO (0.0, 1.0))




generateCSV :: Int -> Int -> String -> IO ()
generateCSV n m filename = do
    rows <- replicateM n (generateRandomRow m)
    let csvContent = unlines $ map (intercalate "," . map show) rows
    writeFile filename csvContent



genCSV n m file = do
  let fullpath = ("src/Data/" ++ file)
    in generateCSV n m fullpath
