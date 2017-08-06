import Control.Concurrent.Async
import Control.Monad (replicateM)
import System.Environment (getArgs)

import AI
import Online

main = do
    [ni, iterations, server, port] <- getArgs
    let numInstances = read ni

    let names = ["Specimen" ++ show i | i <- [1 .. numInstances]]
    let defaultPunter name = Player name dfsStrategy
    let startPunters = map defaultPunter names
    sockets <- replicateM numInstances (client server (read port))
    (results:_) <- mapConcurrently (uncurry play) (zip startPunters sockets)
    let top = maximum results
    putStrLn $ "Result: " ++ show results
    putStrLn $ "Top: " ++ show top
