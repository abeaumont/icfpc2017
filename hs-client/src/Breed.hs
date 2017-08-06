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
type SiteDistMap = M.Map Site Sc

geneticStrategy :: Chromosome -> Strategy
geneticStrategy (Chr (a:b:c:_)) gs@GS {punter = p, mines = mines, rivers = rivers, claimed = claimed} =
    Claim p bestOption where
        allSites, availSites :: S.Set Site
        availSites = S.fromList $ sitesOf $ S.toList rivers
        allSites = S.union availSites $ S.fromList $ sitesOf $ M.keys claimed

        fullGraph :: Gr () ()
        fullGraph = undir $ mkUGraph (S.toList allSites) $ S.toList rivers ++ M.keys claimed

        freeGraph :: Gr () ()
        freeGraph = undir $ mkUGraph (S.toList availSites) $ S.toList rivers

        --myGraph :: Gr () ()
        --myGraph = undir $ mkUGraph (S.toList availSites) $ S.toList $ rivers

        ourClaimed :: [River]
        ourClaimed = M.keys $ M.filter (== p) claimed

        reachable :: S.Set Site
        reachable = S.fromList $ sitesOf ourClaimed

        goodRiver (s, t) = s `S.member` reachable || t `S.member` reachable || s `S.member` mines || t `S.member` mines

        nextRivers = S.filter goodRiver rivers
        options = [(r, score r) | r <- S.toList nextRivers]

        reachableMines = mines `S.intersection` reachable
        score :: River -> Sc
        score r@(s, t)
            | s `S.member` reachable = fromMaybe 0 $ M.lookup t finalScore
            | t `S.member` reachable = fromMaybe 0 $ M.lookup s finalScore
            | s `S.member` mines || t `S.member` mines = a
            | otherwise = 0

        scoreMine :: Site -> SiteDistMap
        scoreMine mine = M.fromList $ map (second (fromIntegral . (^2))) $ level mine fullGraph

        scoreMines :: SiteDistMap
        scoreMines = foldl' (M.unionWith max) M.empty $ map scoreMine $ S.toList reachableMines

        finalScore :: SiteDistMap
        finalScore = M.mapWithKey (\k v -> b * v + c * sum (map (scoreMines M.!) $ neighbors freeGraph k)) scoreMines

        bestOption = if null options then head (S.toList rivers) else fst $ maximumBy (comparing snd) options
