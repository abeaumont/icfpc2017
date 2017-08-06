module Main where

import System.Environment (getArgs)
import System.IO

import AI
import Online

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    [host, port] <- getArgs
    sock <- client host (read port)
    scores <- play (Player "hspunter" dfsStrategy) sock
    print scores
