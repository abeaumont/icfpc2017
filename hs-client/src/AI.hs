module AI where

import Data.List (foldl', maximumBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S

import Lib

import Debug.Trace

type Strategy = GameState -> Move

mockStrategy :: Strategy
mockStrategy gs@GS {punter = p, rivers = rivers} = Claim p $ S.findMin rivers

adjacentSite :: River -> Site -> Bool
adjacentSite (River s t) n = n == s || n == t

adjacentRivers :: River -> River -> Bool
adjacentRivers (River s1 t1) (River s2 t2) =
    s1 == s2 || s1 == t2 || t1 == s2 || t1 == t2

type Sc = Int
type Dist = Int

type Graph a = M.Map River a
type DistMap = Graph Dist

adjacent :: S.Set River -> Site -> [River]
adjacent gr site = filter (`adjacentSite` site) $ S.toList gr

allSites :: [River] -> [Site]
allSites [] = []
allSites (River s t : rs) = s:t:allSites rs

dfsStrategy :: Strategy
dfsStrategy gs@GS {punter = p, mines = mines, rivers = rivers, claimed = claimed} =
    Claim p $ fst $ maximumBy (comparing snd) (traceShow options options) where
        options = [(r, score r) | r <- S.toList rivers]

        score :: River -> Sc
        score r@(River s t)
            | S.null reachableMines = fromMaybe 0 (M.lookup r startMap)
            | s `S.member` reachable || t `S.member` reachable = fromMaybe 0 (M.lookup r distances)
            | otherwise = 0

        reachable :: S.Set Site
        reachable = S.fromList $ allSites ours

        ours = M.keys $ M.filter (== p) claimed

        reachableMines = mines `S.intersection` reachable

        startRivers = filter (`M.notMember` claimed) $ concatMap (adjacent rivers) $ S.toList mines

        startMap :: DistMap
        startMap = M.fromList [(r, 1) | r <- startRivers]

        scoreMine :: Site -> DistMap
        scoreMine mine = runBFS 1 (S.singleton mine) S.empty rivers M.empty where
            runBFS :: Dist -> S.Set Site -> S.Set River -> S.Set River -> DistMap -> DistMap
            runBFS dist border visited rest m
                | null nextStep = m
                | otherwise = runBFS (dist + 1) border' visited' rest' m' where
                nextStep :: S.Set River
                nextStep = S.fromList (concatMap (adjacent rest) $ S.toList border) S.\\ visited
                ns = S.toList nextStep
                newSites = allSites ns
                border' = S.fromList newSites S.\\ border
                visited' = S.union visited nextStep
                rest' = rest S.\\ nextStep
                m'  = M.unionWith min m $ M.fromList $ zip ns (repeat dist)

        distances :: DistMap
        distances = foldl' (M.unionWith max) M.empty $ map scoreMine $ S.toList reachableMines
