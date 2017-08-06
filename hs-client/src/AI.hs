module AI
    ( Strategy, mockStrategy, simpleStrategy, allSites
    ) where

import Data.List (foldl', maximumBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S

import Data.Graph.Inductive
import Data.Graph.Inductive.Query
import Lib

import Debug.Trace

type Strategy = GameState -> Move

mockStrategy :: Strategy
mockStrategy gs@GS {punter = p, rivers = rivers} = Claim p $ S.findMin rivers

type Sc = Int
type Dist = Int
type DistMap = M.Map River Dist
type SiteDistMap = M.Map Site Dist

allSites :: [River] -> [Site]
allSites [] = []
allSites ((s, t) : rs) = s:t:allSites rs

simpleStrategy :: Strategy
simpleStrategy gs@GS {punter = p, mines = mines, rivers = rivers, claimed = claimed} =
    Claim p $ fst $ maximumBy (comparing snd) (traceShowId options) where
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
        startMap = M.fromList [(r, 1) | r <- startRivers]

        scoreMine :: Site -> SiteDistMap
        scoreMine mine = depths where
            depths = M.fromList $ level mine fullGraph

        scoreMines :: SiteDistMap
        scoreMines = foldl' (M.unionWith max) M.empty $ map scoreMine $ S.toList reachableMines

        finalScore :: SiteDistMap
        finalScore = M.mapWithKey (\k v -> sum $ map (scoreMines M.!) $ neighbors freeGraph k) scoreMines
