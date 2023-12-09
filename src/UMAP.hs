module UMAP where 
import Utils.DataGenerator
import Utils.DataIn 
import Utils.DataOut
import Local_spaces.KNN
import SimplicialComplex.Fuzzy
import Optim.OptimizationFunctor
import Spectral_embedding.Spectral 
import Data.Massiv.Array as M

-- LocalFuzzySimplicalSet :: (all data points) -> (single datapoint to find local space) -> Int (amount of neighbors) -> f-set
-- Functor :: points -> local uber metric spaces
-- Functor :: local uber metric space -> fuzzy simplicial complex 

-- top-rep := union of all the f-sets 
-- Functor :: fuzzy simplical complexes -> union of fuzzy sc
-- We recommend the probabilistic t-conorm


-- SpectralEmbeddingInitialization :: top-rep -> Int (dimension of lower space) -> d eigenvectors 
-- Functor :: fuzzy simplicial complex -> fuzzy non-nested flag cover. 

-- OptimizationEmbeddings :: 
-- Functor :: psudeo metric space -> UMAP embedding optimization problem. 



-- umap :: DataPoints -> DataPoints 
umap k x = do
    case x of 
        Just xData ->  
            let amt = getAmt (size xData)
                dm = distanceMatrix xData
                knns = kNearestNeighbors k dm 
                tp = topRep knns amt
            in Just (tp)
        Nothing ->  Nothing


main = do 
    x <- csv2matrix "test1.csv"
    return (umap 3 x)