{-# LANGUAGE FlexibleContexts #-}

module HsBowling.Game (
    Game, players, createGame
) where

import Control.Arrow ((>>>))
import Control.Applicative
import Control.Lens.Getter
import Control.Lens.Operators
import Control.Monad.Reader
import Data.Maybe
import Data.Traversable
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Safe

import qualified Data.List.NonEmpty as N

import HsBowling.Error
import HsBowling.Config
import HsBowling.PlayerName
import HsBowling.Frame
import HsBowling.Player

data Game = G {
    _players :: NonEmpty Player
}

players :: Getter Game (NonEmpty Player)
players = to _players

createGame :: MonadReader Config reader => ValidatedPlayerNames -> reader (Either BowlingError Game)
createGame =
    view getNames
    >>> fmap createPlayer
    >>> sequenceA
    >>> fmap sequenceA
    >>> fmap (fmap (\players -> G { _players = players }))

currentPlayer :: Game -> Player
currentPlayer game =
    let players' = game^.players
        currentFrameNumber = N.head players' & lastFrame & view number
     in fromMaybe (N.head players') $
            (tryFind (lastFrame >>> view number >>> (/= currentFrameNumber)) players') <|>
            (tryFind (not . (lastFrame >>> isFrameFinished)) players')
            where tryFind predicate = N.dropWhile (not . predicate) >>> headMay

rollGame :: MonadReader Config reader => Int -> Game -> reader (Either BowlingError Game)
rollGame score game =
    game^.players
    & fmap (\player ->
        if player^.name == (currentPlayer game)^.name
        then rollPlayer score player
        else return $ Right player)
    & sequenceA
    & fmap sequenceA
    & fmap (fmap (\players -> game { _players = players }))

isGameFinished :: MonadReader Config reader => Game -> reader Bool
isGameFinished game =
    game^.players & N.last & isPlayerFinished
