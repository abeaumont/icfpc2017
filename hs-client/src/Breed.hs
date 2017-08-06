module Breed where

import Control.Arrow (second)
import Control.Monad

import Data.List (foldl', maximumBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import System.Random

import Data.Graph.Inductive
import Data.Graph.Inductive.Query

import AI
import Lib

newtype Chromosome = Chr [Float]
    deriving Show

baseChromosome = Chr [0, 0, 0]

mutate :: Chromosome -> IO Chromosome
mutate (Chr genes) = do
    ks <- replicateM (length genes) $ randomRIO (-2, 2)
    return $ Chr $ zipWith (+) genes ks

offspring :: Chromosome -> Chromosome -> IO Chromosome
offspring (Chr g1) (Chr g2) = do
    ng <- mapM randomRIO $ zip g1 g2
    mutate (Chr ng)

offsprings :: Int -> Chromosome -> Chromosome -> IO [Chromosome]
offsprings n mother father = replicateM n (offspring mother father)

type Sc = Float
type SiteWeights = M.Map Site Sc
type DistMap = M.Map River Sc

geneticStrategy :: Chromosome -> Strategy
geneticStrategy (Chr (a:b:c:_)) gs@GS {punter = p, mines = mines, rivers = rivers, claimed = claimed} =
    Claim p $ fst $ maximumBy (comparing snd) options where
        availSites = S.toList $ S.fromList $ allSites $ S.toList rivers

        freeGraph :: Gr () ()
        freeGraph = undir $ mkUGraph availSites $ S.toList rivers

        fullGraph :: Gr () ()
        fullGraph = undir $ mkUGraph availSites $ S.toList rivers ++ M.keys claimed

        options = [(r, score r) | r <- S.toList rivers]

        score :: River -> Sc
        score r@(s, t)
            | S.null reachableMines = fromMaybe 0 (M.lookup r startMap)
            | s `S.member` reachable = fromMaybe 0 $ M.lookup t finalScore
            | t `S.member` reachable = fromMaybe 0 $ M.lookup s finalScore
            | otherwise = 0

        ours :: [River]
        ours = M.keys $ M.filter (== p) claimed

        reachable :: S.Set Site
        reachable = S.fromList $ allSites ours

        reachableMines = mines `S.intersection` reachable

        startRivers :: [River]
        startRivers = [(s, t) | (s, t) <- S.toList rivers, s `S.member` mines || t `S.member` mines]

        startMap :: DistMap
        startMap = M.fromList [(r, c) | r <- startRivers]

        scoreMine :: Site -> SiteWeights
        scoreMine mine = depths where
            depths = M.fromList $ map (second fromIntegral) $ level mine fullGraph

        scoreMines :: SiteWeights
        scoreMines = foldl' (M.unionWith max) M.empty $ map scoreMine $ S.toList reachableMines

        scoreNeighbours :: SiteWeights
        scoreNeighbours = M.mapWithKey (\k v -> sum $ map (scoreMines M.!) $ neighbors freeGraph k) scoreMines

        scoreSite :: Site -> Sc
        scoreSite s = a * (scoreMines M.! s) + b * (scoreNeighbours M.! s)

        finalScore :: SiteWeights
        finalScore = M.mapWithKey (\k v -> scoreSite k) scoreMines
