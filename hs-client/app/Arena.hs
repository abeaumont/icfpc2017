module Main where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad (liftM3, mapM, replicateM)
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.IO

import Breed
import AI
import Lib
import Online

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf i xs = c:chunksOf i cs where (c, cs) = splitAt i xs

main = do
    [pl, ps, it, server, port] <- getArgs
    manager <- mkManager server (read port)
    let poolSize = read pl; iterations = read it; numPlayers = read ps
    --sockets <- replicateM numInstances (mkSocket manager)
    --socket <- mkSocket manager >>= newMVar
    --threadedPlay <- mkSocket manager >>= newMVar >>= flip withMVar (`play` player)
    --let playerMakers = map Player sockets :: [Name -> Strategy -> Player]

    startPool <- replicateM poolSize (mutate baseChromosome)

    hSetBuffering stdout LineBuffering
    let runGeneration i pool | i == iterations = return ()
                             | otherwise = do
            putStrLn $ "Generation " ++ show i
            let names = ["Specimen" ++ show i ++ [c] | c <- take poolSize ['A'..]]
                strategies = map geneticStrategy pool
                players = zipWith Player names strategies

            let playParty party = do
                    sockets <- replicateM numPlayers (mkSocket manager)
                    allResults@((_, scoreTable):_) <- mapConcurrently (uncurry play) (zip sockets party)
                    let scoreMap = M.fromList $ map (\(ScoreOf p s) -> (p, s)) scoreTable
                    let ids = map fst allResults
                    return $ map (scoreMap M.!) ids

            results <- concat `fmap` mapM playParty (chunksOf numPlayers players)

            let top2 = take 2 $ sortBy (flip $ comparing snd) $ zip pool results
            let parents@[mother, father] = map fst top2
            putStrLn $ "Result: " ++ show results
            putStrLn $ "Median: " ++ show (fromIntegral (sum results) / fromIntegral (length results) :: Float)
            putStrLn $ "Top2: " ++ show parents

            newPool <- offsprings (poolSize - 2) mother father
            runGeneration (i + 1) (newPool ++ parents)

    runGeneration 1 startPool
