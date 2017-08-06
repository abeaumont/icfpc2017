module Online where

import Data.Char (digitToInt)
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
recvLength sock = run 0 where
    run acc = do
        char <- recv sock 1
        case unpack char of
            ":" -> return acc
            [d] -> run $ 10 * acc + digitToInt d
            r -> error $ "Error: [" ++ show r ++ "]"

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

type Name = String
data Player = Player Name Strategy

type GameResult = (Punter, ScoreTable)
play :: Socket -> Player -> IO GameResult
play sock (Player name strategy) = do
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
            MessageEnd (Endgame moves scores) -> do
                close sock
                return (punter gs, scores)
    }
    makeTurn initGS

data ConnectionManager = CM Family SockAddr

mkManager :: String -> Int -> IO ConnectionManager
mkManager host port = do
    (serverAddr:_) <- getAddrInfo Nothing (Just host) (Just $ show port)
    return $ CM (addrFamily serverAddr) (addrAddress serverAddr)

mkSocket :: ConnectionManager -> IO Socket
mkSocket (CM family addr) = do
    sock <- socket family Stream defaultProtocol
    connect sock addr
    return sock

connectSocket :: ConnectionManager -> Socket -> IO ()
connectSocket (CM family addr) sock = connect sock addr
