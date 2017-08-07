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
type River = (Site, Site)

data GameState = GS
    { punter :: !Punter
    , punters :: !Int
    , mines :: !(S.Set Site)
    , rivers :: !(S.Set River)
    , claimed :: !(M.Map River Punter)
    , options :: !(M.Map River Punter)
    }

data JsonRiver = JR Site Site
instance FromJSON JsonRiver where
    parseJSON = withObject "River" $ \v -> JR <$> v .: "source" <*> v .: "target"

instance FromJSON GameState where
    parseJSON = withObject "GameState" $ \v -> do
        mapObj <- v .: "map"
        punter <- v .: "punter"
        punters <- v .: "punters"
        mines <- mapObj .: "mines"
        jrivers <- mapObj .: "rivers"
        let rivers = map (\(JR s t) -> (s, t)) jrivers
        return $ GS punter punters (S.fromList mines) (S.fromList rivers) M.empty M.empty

newtype Handshake = Handshake String
    deriving Show

instance FromJSON Handshake where
    parseJSON = withObject "Handshake" $ \v -> Handshake <$> v .: "you"

data Move = Claim !Punter !River | Pass !Punter | Option !Punter !River
instance FromJSON Move where
    parseJSON = withObject "Move" $ \v -> (do
        move <- v .: "claim"
        punter <- move .: "punter"
        source <- move .: "source"
        target <- move .: "target"
        return $ Claim punter (source, target))
        <|> (do
        move <- v .: "pass"
        punter <- move .: "punter"
        return $ Pass punter)
        <|> (do
        move <- v .: "option"
        punter <- move .: "punter"
        source <- move .: "source"
        target <- move .: "target"
        return $ Option punter (source, target))

instance ToJSON Move where
    toJSON (Claim punter (source, target)) =
        object ["claim" .= object ["punter" .= punter, "source" .= source, "target" .= target]]
    toJSON (Option punter (source, target)) =
        object ["claim" .= object ["punter" .= punter, "source" .= source, "target" .= target]]
    toJSON (Pass punter) =
        object ["pass" .= object ["punter" .= punter]]

data ScoreOf = ScoreOf !Punter !Int
    deriving (Eq, Show)

instance Ord ScoreOf where
    compare (ScoreOf _ s1) (ScoreOf _ s2) = compare s1 s2

instance FromJSON ScoreOf where
    parseJSON = withObject "ScoreOf" $ \v ->
        ScoreOf <$> v .: "punter" <*> v .: "score"

type ScoreTable = [ScoreOf]

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
makeMove gs@GS {rivers = rivers, options = options} (Option p r) =
    gs {rivers = S.delete r rivers, options = M.insert r p options}

strategy :: GameState -> Move
strategy gs@GS {punter = p, rivers = rivers} = Claim p $ S.findMin rivers
