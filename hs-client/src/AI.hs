module AI
    ( Strategy, mockStrategy, simpleStrategy, sitesOf
    ) where

import Control.Arrow (second)
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

sitesOf :: [River] -> [Site]
sitesOf [] = []
sitesOf ((s, t) : rs) = s:t:sitesOf rs

simpleStrategy :: Strategy
simpleStrategy gs@GS {punter = p, mines = mines, rivers = rivers, claimed = claimed} =
    Claim p (traceShow scoreMines bestOption) where
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
            | s `S.member` mines || t `S.member` mines = 1
            | otherwise = 0

        scoreMine :: Site -> SiteDistMap
        scoreMine mine = M.fromList $ map (second (^2)) $ level mine fullGraph

        scoreMines :: SiteDistMap
        scoreMines = foldl' (M.unionWith max) M.empty $ map scoreMine $ S.toList reachableMines

        finalScore :: SiteDistMap
        finalScore = M.mapWithKey (\k v -> v + sum (map (scoreMines M.!) $ neighbors freeGraph k)) scoreMines

        bestOption = if null options then head (S.toList rivers) else fst $ maximumBy (comparing snd) options
