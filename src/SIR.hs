{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module SIR
    ( epidemicThreshold
    , spreadingPower
    ) where
--------------------------------------------------------------------------------
import Control.Monad               (replicateM, filterM, liftM)
import Control.Monad.State         (State, evalState, gets, put)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Map.Strict             (Map)
import Data.Set                    (Set)
import Model
import System.Random               (Random, RandomGen, StdGen, mkStdGen, random)
--------------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
--------------------------------------------------------------------------------

epidemicThreshold :: Graph -> Double 
epidemicThreshold Graph{..} =
    let numNodes      = fromIntegral (M.size neighbours)
        averageDegree = (fromIntegral . sum . map S.size               . M.elems $ neighbours) / numNodes
        secondMoment  = (fromIntegral . sum . map (\s -> S.size s ^ 2) . M.elems $ neighbours) / numNodes
    in averageDegree / (secondMoment - averageDegree)


-- spreadingPower :: Int -> Graph -> Double -> State StdGen (Map Node Double)
-- spreadingPower n Graph{..} infectionProbability = do
--     let nodes = M.keys neighbours
--     gens <- fmap mkStdGen <$> getRandomNumbers (length nodes)
--     let computation (u, gen) = evalState (runSir n u) gen
--         spreadingPowers = parMap rdeepseq computation (nodes `zip` gens)
--     return . M.fromList $ nodes `zip` spreadingPowers

--     where
--         runSir :: Int -> Node -> State StdGen Double
--         runSir n u = do
--             let s = u `S.delete` M.keysSet neighbours
--                 i = S.singleton u
--                 r = S.empty
--             powers <- replicateM n (sir s i r)
--             return $ fromIntegral (sum powers) / fromIntegral n

--         sir :: Set Node -> Set Node -> Set Node -> State StdGen Int
--         sir !s !i !r | S.null i  = return (S.size r)
--                      | otherwise = do
--                            let infectable = [ w
--                                             | v <- S.toList i
--                                             , w <- S.toList (neighbours M.! v)
--                                             , w `S.member` s
--                                             ]
--                            xs <- getRandomNumbers (length infectable)
--                            let i' = S.fromList [ w | (w,x) <- infectable `zip` xs, x < infectionProbability ]
--                                r' = r `S.union` i
--                                s' = s `S.difference` i'
--                            sir s' i' r'


spreadingPower :: Graph -> Node -> Int -> Double -> State StdGen Double
spreadingPower g u n infectionProbability = do
    gens <- fmap mkStdGen <$> getRandomNumbers n
    let ress = parMap rdeepseq (evalState (sir g u infectionProbability)) gens
    return $ fromIntegral (sum ress) / fromIntegral n


sir :: Graph -> Node -> Double -> State StdGen Int
sir Graph{..} u infectionProbability =
    if u `M.member` neighbours
        then let s = u `S.delete` M.keysSet neighbours
                 i = S.singleton u
                 r = S.empty
             in go s i r
        else return 0

    where
        go :: Set Node -> Set Node -> Set Node -> State StdGen Int
        go !s !i !r | S.null i  = return (S.size r)
                    | otherwise = do
                        let infectable = concat 
                                       . S.map (S.toList . S.filter (`S.member` s) . (neighbours M.!))  
                                       $ i
                        i' <- S.fromList <$> filterM infect infectable
                        let r' = r `S.union` i
                            s' = s `S.difference` i'

                        go s' i' r'

        infect :: Node -> State StdGen Bool
        infect _ = do
            x <- getRandomNumber
            return (x < infectionProbability)


getRandomNumber :: (Random a, RandomGen g) => State g a 
getRandomNumber = do
    (x, gen) <- gets random
    put gen
    return x

getRandomNumbers :: (Random a, RandomGen g) => Int -> State g [a]
getRandomNumbers n = replicateM n getRandomNumber