module Main where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad (liftM3, mapM, replicateM)
import Data.List (sortBy)
import System.Environment (getArgs)
import System.IO

import Breed
import AI
import Lib
import Online

punterOf :: ScoreOf -> Punter
punterOf (ScoreOf p _) = p

main = do
    [ni, it, server, port] <- getArgs
    manager <- mkManager server (read port)
    let numInstances = read ni; iterations = read it
    --sockets <- replicateM numInstances (mkSocket manager)
    --socket <- mkSocket manager >>= newMVar
    --threadedPlay <- mkSocket manager >>= newMVar >>= flip withMVar (`play` player)
    --let playerMakers = map Player sockets :: [Name -> Strategy -> Player]

    startPool <- replicateM numInstances (mutate baseChromosome)

    hSetBuffering stdout LineBuffering
    let runGeneration i pool | i == iterations = return ()
                             | otherwise = do
            putStrLn $ "Generation " ++ show i
            sockets <- replicateM numInstances (mkSocket manager)
            let names = ["Specimen" ++ show i ++ [c] | c <- take numInstances ['A'..]]
                strategies = map geneticStrategy pool
                --players = getZipList $ ZipList playerMakers <*> ZipList names <*> ZipList strategies
                players = zipWith Player names strategies

            (results:_) <- map snd `fmap` mapConcurrently (uncurry play) (zip sockets players)
            let top2 = take 2 $ sortBy (flip compare) results
            let parents@[mother, father] = map ((pool !!) . punterOf) top2
            putStrLn $ "Result: " ++ show results
            putStrLn $ "Top2: " ++ show parents

            newPool <- offsprings (numInstances - 2) mother father
            runGeneration (i + 1) (newPool ++ parents)

    runGeneration 1 startPool
