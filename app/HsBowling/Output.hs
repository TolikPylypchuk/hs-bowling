{-# LANGUAGE FlexibleContexts #-}

module HsBowling.Output (
    formatRoll, formatScore, formatPlayer, formatGame, formatError
) where

import Control.Applicative
import Control.Arrow ((>>>))
import Control.Lens.Getter
import Control.Lens.Operators
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import qualified Data.List.NonEmpty as N
import Data.Maybe

import HsBowling.Error
import HsBowling.Config
import HsBowling.PlayerName
import HsBowling.Frame
import HsBowling.Player
import HsBowling.Game

add :: MonadWriter String writer => String -> writer ()
add = tell

addLine :: MonadWriter String writer => String -> writer ()
addLine = add . (++ "\n")

pad :: Int -> String -> String
pad num str = replicate (num - length str) ' ' ++ str

formatRoll :: Int -> String
formatRoll score =
    if score == 0 then "- " else pad 2 $ show score

formatScore :: MonadReader Config reader => Bool -> FrameScore -> reader String
formatScore isLastFrame score = do
    numPins <- asks $ view numberOfPins
    return $ execWriter $ do
        let format roll = case roll of
                            Just roll' | roll' == numPins -> "X "
                            Just roll' -> formatRoll roll'
                            Nothing -> "  "
        
        add $ format $ score^.firstRollScore

        let secondRoll = case (score^.firstRollScore, score^.secondRollScore) of
                            (Just roll1, Just roll2) | roll1 + roll2 == numPins -> "/ "
                            (Just roll1, Just roll2) | roll1 == numPins && roll2 == numPins -> "X "
                            (_, Just roll) -> formatRoll roll
                            (_, Nothing) -> "  "
        
        add $ "|" ++ secondRoll

        when isLastFrame $ add $ "|" ++ (format $ score^.thirdRollScore)

formatPlayer :: MonadReader Config reader => Player -> reader String
formatPlayer player = do
    config <- ask
    totalScores <- player^.frames & N.toList & getTotalScores

    return $ execWriter $ do
        addLine $ player^.name^.get

        let numFrames = config^.numberOfFrames

        let indexedTotalScores = zip [0..] totalScores

        let firstLine = indexedTotalScores
                        & map (uncurry $ \index ->
                            view totalScore
                            >>> fmap show
                            >>> fromMaybe ""
                            >>> pad (if index + 1 == numFrames then 8 else 5))
                        & intercalate "|"

        let intermediateLine = join $ replicate (length firstLine + 2) "-"

        addLine intermediateLine
        addLine $ "|" ++ firstLine ++ "|"
        addLine intermediateLine

        let secondLine = indexedTotalScores
                         & map (uncurry $ \index -> formatScore $ index + 1 == numFrames)
                         & sequenceA
                         & flip runReader config
                         & intercalate "|"

        addLine $ "|" ++ secondLine ++ "|"
        addLine intermediateLine

formatGame :: MonadReader Config reader => Game -> reader String
formatGame =
    view players
    >>> fmap formatPlayer
    >>> sequenceA
    >>> fmap N.toList
    >>> (fmap $ intercalate "\n")

formatError :: MonadReader Config reader => BowlingError -> reader String
formatError error =
    ask <&> \config ->
        case error of
            InvalidFrameNumber num ->
                show num ++ " is an invalid frame number."
            PlayerNameEmpty ->
                "The player's name is empty."
            PlayerNameTooLong _ ->
                "The name is too long. A player's name should not exceed " ++ maxLength ++ " characters."
                where maxLength = show $ config^.maxNameLength & fromMaybe 0
            PlayerListEmpty ->
                "The player list is empty"
            TooManyPlayers count ->
                show count ++ " is too many players. The number of players must be at most " ++ maxCount ++ "."
                where maxCount = show $ config^.maxPlayerCount & fromMaybe 0
            DuplicatePlayers players ->
                case N.tail players of
                    [] ->
                        "The name " ++ (N.head players) ++ " is duplicated."
                    _ ->
                        players
                        & N.toList
                        & intercalate ", "
                        & ("The names " ++)
                        & (++ " are duplicated.")
            InvalidScore score ->
                show score ++ " is an invalid score. A score of a frame must be less than or equal to " ++ numPins ++ "."
                where numPins = show $ config^.numberOfPins
            RollAfterLastFrame ->
                "A player rolled past the last frame."
        
