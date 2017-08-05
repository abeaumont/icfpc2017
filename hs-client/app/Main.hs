module Main where

import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy.Char8 (unpack)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import System.Environment (getArgs)

import Lib

sendJSON :: ToJSON a => Socket -> a -> IO ()
sendJSON sock v = do
    let msg = encode v
    putStr "Sending "
    putStrLn $ unpack msg
    send sock $ toStrict msg
    return ()

recvJSON :: FromJSON a => Socket -> IO a
recvJSON sock = do
    msg <- recv sock 4096
    let Just res = decodeStrict msg
    return res

play :: GameState -> Socket -> IO [ScoreOf]
play gs sock = do
    sendJSON sock (Pass $ punter gs)
    msg <- recvJSON sock
    case msg of
        MessageMove moves -> play (makeMoves gs moves) sock
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
    scores <- play initGS sock
    print scores

main :: IO ()
main = do
    [name, host, port] <- getArgs
    client name host (read port)
