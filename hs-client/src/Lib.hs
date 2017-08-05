{-# LANGUAGE OverloadedStrings #-}

module Lib
    where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S

handshake name = object ["me" .= name]

type Punter = Int
noone = 0 :: Punter
type Site = Int
data River = River !Site !Site
    deriving (Eq, Ord)
instance FromJSON River where
    parseJSON = withObject "River" $ \v -> River <$> v .: "source" <*> v .: "target"

type Map = M.Map River Punter

data GameState = GS
    { punter :: !Punter
    , punters :: ![Punter]
    , mines :: S.Set Site
    , rivers :: !Map
    }

instance FromJSON GameState where
    parseJSON = withObject "GameState" $ \v -> makeGS
        <$> v .: "punter"
        <*> v .: "punters"
        <*> v .: "map"

newtype Handshake = Handshake String
    deriving Show

instance FromJSON Handshake where
    parseJSON = withObject "Handshake" $ \v -> Handshake <$> v .: "you"

data Move = Claim !Punter !River | Pass !Punter
instance FromJSON Move where
    parseJSON = withObject "Move" $ \v -> (Claim
        <$> v .: "punter"
        <*> (River <$> v.: "source" <*> v .: "target"))
        <|> (Pass
        <$> v .: "punter")

instance ToJSON Move where
    toJSON (Claim punter (River source target)) =
        object ["claim" .= object ["punter" .= punter, "source" .= source, "target" .= target]]
    toJSON (Pass punter) =
        object ["pass" .= punter]

data ScoreOf = ScoreOf !Punter !Int
    deriving Show
instance FromJSON ScoreOf where
    parseJSON = withObject "ScoreOf" $ \v ->
        ScoreOf <$> v .: "punter" <*> v .: "score"

data Endgame = Endgame [Move] [ScoreOf]
instance FromJSON Endgame where
    parseJSON = withObject "Endgame" $ \v ->
        Endgame <$> v .: "moves" <*> v .: "scores"

data Message = MessageMove [Move] | MessageEnd Endgame
instance FromJSON Message where
    parseJSON = withObject "Message" $ \v -> (do
        moveObj <- v .: "move"
        movesObj <- moveObj .: "moves"
        moves <- parseJSON movesObj
        return $ MessageMove moves)
        <|>
        (MessageEnd <$> v .: "stop")

parseMap :: Value -> Parser ([Site], [River])
parseMap = withObject "Map" $ \v -> (,)
    <$> v .: "mines"
    <*> v .: "rivers"

makeGS :: Punter -> [Punter] -> ([Site], [River]) -> GameState
makeGS p ps (ms, rs) = GS p ps (S.fromList ms) (M.fromList $ zip rs (repeat 0))

makeMoves :: GameState -> [Move] -> GameState
makeMoves = foldl' makeMove

makeMove :: GameState -> Move -> GameState
makeMove gs (Pass _) = gs
makeMove gs@GS {rivers = rivers} (Claim p r) = gs {rivers = M.insert r p rivers}
