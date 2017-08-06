module Online where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS (toStrict)
import qualified Data.ByteString as BS (concat, length, tail)
import Data.ByteString.Char8 (append, cons, pack, unpack, readInt)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)

import AI
import Lib

sendJSON :: ToJSON a => Socket -> a -> IO ()
sendJSON sock v = do
    let msg = BS.toStrict $ encode v
    let rawmsg = append (pack $ show (BS.length msg)) $ cons ':' msg
    send sock rawmsg
    return ()

recvLength :: Socket -> IO Int
recvLength sock = run "" where
    run acc = do
        char <- recv sock 1
        case unpack char of
            ":" -> return $ read $ reverse acc
            [d] -> run (d:acc)

recvTotal sock len = run len [] where
    run 0 acc = return $ BS.concat $ reverse acc
    run len acc = do
        chunk <- recv sock len
        run (len - BS.length chunk) (chunk:acc)

recvJSON :: FromJSON a => Socket -> IO a
recvJSON sock = do
    buffer <- recvLength sock
    msg <- recvTotal sock buffer
    case eitherDecodeStrict msg of
        Left err -> error err
        Right res -> return res

play :: Player -> Socket -> IO ScoreTable
play (Player name strategy) sock = do
    sendJSON sock (handshake name)
    response <- recvJSON sock :: IO Handshake
    initGS <- recvJSON sock
    sendJSON sock (ready $ punter initGS)

    let makeTurn gs = do {
        msg <- recvJSON sock;
        case msg of
            MessageMove moves -> do
                let newGS = makeMoves gs moves
                sendJSON sock (strategy newGS)
                makeTurn newGS
            MessageEnd (Endgame moves scores) -> return scores
    }

    makeTurn initGS

client :: String -> Int -> IO Socket
client host port = withSocketsDo $ do
    (serverAddr:_) <- getAddrInfo Nothing (Just host) (Just $ show port)
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    return sock
