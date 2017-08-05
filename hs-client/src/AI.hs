module AI where

import Data.List (maximumBy)
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
type DistMap = M.Map River Dist

dfsStrategy :: Strategy
dfsStrategy gs@GS {punter = p, mines = mines, rivers = rivers, claimed = claimed} =
    Claim p $ fst $ maximumBy (comparing snd) (traceShow options options) where
        options = [(r, score r) | r <- S.toList rivers]

        score :: River -> Sc
        score r | any (adjacentRivers r) ours = fromMaybe 1 (M.lookup r distances)
                | otherwise = 0

        ours :: [River]
        ours = M.keys $ M.filter (== p) claimed

        distances :: DistMap
        distances = runDFS 1 initRivers M.empty where
            initRivers :: [River]
            initRivers = S.toList $ S.filter initRiver rivers

            initRiver :: River -> Bool
            initRiver r = any (adjacentSite r) (S.toList mines) && M.notMember r claimed

            runDFS :: Dist -> [River] -> DistMap -> DistMap
            runDFS _ [] m = m
            runDFS d rs m = runDFS (d + 1) rs' m' where
                m' = M.unionWith min m (M.fromList $ map (\r -> (r, d)) rs)
                rs' :: [River]
                rs' = [r | (r, d) <- M.toList m', anyAdj r d]
                anyAdj :: River -> Dist -> Bool
                anyAdj r dd = any (adjacentRivers r) rs && M.notMember r claimed && M.notMember r m
