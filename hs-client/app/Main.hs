module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS (toStrict)
import qualified Data.ByteString as BS (length, tail)
import Data.ByteString.Char8 (append, cons, pack, unpack, readInt)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import System.Environment (getArgs)
import System.IO

import AI
import Lib

sendJSON :: ToJSON a => Socket -> a -> IO ()
sendJSON sock v = do
    let msg = BS.toStrict $ encode v
    let rawmsg = append (pack $ show (BS.length msg)) $ cons ':' msg
    putStr "Sending "
    putStrLn $ unpack rawmsg
    send sock rawmsg
    return ()

recvJSON :: FromJSON a => Socket -> IO a
recvJSON sock = do
    let buffer = 1048576
    rawmsg <- recv sock buffer
    let Just (_, withColon) = readInt rawmsg
    let msg = BS.tail withColon
    putStr "Receiving "
    putStrLn $ unpack msg
    let Just res = decodeStrict msg
    return res

play :: Strategy -> Socket -> GameState -> IO [ScoreOf]
play strategy sock gs = do
    msg <- recvJSON sock
    case msg of
        MessageMove moves -> do
            let newGS = makeMoves gs moves
            sendJSON sock (strategy newGS) --TODO: make some strategy
            play strategy sock newGS
        MessageEnd (Endgame moves scores) -> return scores

client :: String -> String -> Int -> IO ()
client name host port = withSocketsDo $ do
    (serverAddr:_) <- getAddrInfo Nothing (Just host) (Just $ show port)
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    sendJSON sock (handshake name)
    response <- recvJSON sock
    print (response :: Handshake)
    initGS <- recvJSON sock
    sendJSON sock (ready $ punter initGS)
    scores <- play dfsStrategy sock initGS
    print scores

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    [name, host, port] <- getArgs
    client name host (read port)
