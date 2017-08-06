module Main where

import System.Environment (getArgs)
import System.IO

import AI
import Online

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    [host, port] <- getArgs
    cm <- mkManager host (read port)
    sock <- mkSocket cm
    scores <- play sock (Player "hspunter" simpleStrategy)
    print scores
