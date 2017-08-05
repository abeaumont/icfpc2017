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
ready punter = object ["ready" .= punter]

type Punter = Int
noone = 0 :: Punter
type Site = Int
data River = River !Site !Site
    deriving (Eq, Ord)

instance Show River where
    show (River s t) = show s ++ "->" ++ show t

mkRiver s t | s < t = River s t
            | otherwise = River t s

instance FromJSON River where
    parseJSON = withObject "River" $ \v -> River <$> v .: "source" <*> v .: "target"

data GameState = GS
    { punter :: !Punter
    , punters :: !Int
    , mines :: !(S.Set Site)
    , rivers :: !(S.Set River)
    , claimed :: !(M.Map River Punter)
    }

instance FromJSON GameState where
    parseJSON = withObject "GameState" $ \v -> do
        mapObj <- v .: "map"
        punter <- v .: "punter"
        punters <- v .: "punters"
        mines <- mapObj .: "mines"
        rivers <- mapObj .: "rivers"
        return $ GS punter punters (S.fromList mines) (S.fromList rivers) M.empty

newtype Handshake = Handshake String
    deriving Show

instance FromJSON Handshake where
    parseJSON = withObject "Handshake" $ \v -> Handshake <$> v .: "you"

data Move = Claim !Punter !River | Pass !Punter
instance FromJSON Move where
    parseJSON = withObject "Move" $ \v -> (do
        move <- v .: "claim"
        punter <- move .: "punter"
        source <- move .: "source"
        target <- move .: "target"
        return $ Claim punter (River source target))
        <|> (do
        move <- v .: "pass"
        punter <- move .: "punter"
        return $ Pass punter)

instance ToJSON Move where
    toJSON (Claim punter (River source target)) =
        object ["claim" .= object ["punter" .= punter, "source" .= source, "target" .= target]]
    toJSON (Pass punter) =
        object ["pass" .= object ["punter" .= punter]]

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

makeMoves :: GameState -> [Move] -> GameState
makeMoves = foldl' makeMove

makeMove :: GameState -> Move -> GameState
makeMove gs (Pass _) = gs
makeMove gs@GS {rivers = rivers, claimed = claimed} (Claim p r) =
    gs {rivers = S.delete r rivers, claimed = M.insert r p claimed}

strategy :: GameState -> Move
strategy gs@GS {punter = p, rivers = rivers} = Claim p $ S.findMin rivers
