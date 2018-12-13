{-# LANGUAGE FlexibleContexts #-}

module HsBowling.Frame (
    FrameState, Frame, FrameScore, state, number, totalScore, firstRollScore, secondRollScore, thirdRollScore,
    createFrame, isFrameFinished, isLast, rollFrame, getScores, getTotalScores
) where

import Control.Lens.Getter
import Control.Lens.Operators
import Control.Monad.Reader
import Data.Maybe
import Safe

import HsBowling.Error
import HsBowling.Config

data FrameState = NotStarted
                | InProgress Int
                | Open (Int, Int)
                | Strike
                | LastStrikeInProgress1
                | LastStrikeInProgress2 Int
                | LastStrike (Int, Int)
                | Spare Int
                | LastSpareInProgress Int
                | LastSpare (Int, Int)
                deriving (Eq, Show)

data Frame = F {
    _state :: FrameState,
    _number :: Int
} deriving (Eq, Show)

data FrameScore = FS {
    _total :: Maybe Int,
    _firstRoll :: Maybe Int,
    _secondRoll :: Maybe Int,
    _thirdRoll :: Maybe Int
} deriving (Eq, Show)

state :: Getter Frame FrameState
state = to _state

number :: Getter Frame Int
number = to _number

totalScore :: Getter FrameScore (Maybe Int)
totalScore = to _total

firstRollScore :: Getter FrameScore (Maybe Int)
firstRollScore = to _firstRoll

secondRollScore :: Getter FrameScore (Maybe Int)
secondRollScore = to _secondRoll

thirdRollScore :: Getter FrameScore (Maybe Int)
thirdRollScore = to _thirdRoll

frameScore :: FrameScore
frameScore = FS {
    _total = Nothing,
    _firstRoll = Nothing,
    _secondRoll = Nothing,
    _thirdRoll = Nothing
}

createFrame :: MonadReader Config reader => Int -> reader (Either BowlingError Frame)
createFrame num =
    ask <&> \config ->
        if num > 0 && num <= config^.numberOfFrames
        then Right $ F { _state = NotStarted, _number = num }
        else Left $ InvalidFrameNumber num

isFrameFinished :: Frame -> Bool
isFrameFinished frame =
    case frame^.state of
        Open _       -> True
        Strike       -> True
        LastStrike _ -> True
        Spare _      -> True
        LastSpare _  -> True
        _            -> False

isLast :: MonadReader Config reader => Frame -> reader Bool
isLast frame =
    asks (view numberOfFrames) <&> \numFrames -> frame^.number == numFrames

rollForNotStarted :: MonadReader Config reader => Int -> Frame -> reader Frame
rollForNotStarted score frame =
    ask <&> \config ->
        if score < config^.numberOfPins then
            frame { _state = InProgress score }
        else
            if frame^.number /= config^.numberOfFrames
            then frame { _state = Strike }
            else frame { _state = LastStrikeInProgress1 }

rollForInProgress :: MonadReader Config reader => Int -> Int -> Frame -> reader Frame
rollForInProgress firstScore secondScore frame =
    ask <&> \config ->
        if (firstScore + secondScore) < config^.numberOfPins then
            frame { _state = Open (firstScore, secondScore) }
        else
            if frame^.number /= config^.numberOfFrames
            then frame { _state = Spare firstScore }
            else frame { _state = LastSpareInProgress firstScore }

rollForFinished :: MonadReader Config reader => Int -> Frame -> reader Frame
rollForFinished score frame =
    return F { _state = InProgress score, _number = frame^.number + 1 }

rollForLastStrikeInProgress1 :: MonadReader Config reader => Int -> Frame -> reader Frame
rollForLastStrikeInProgress1 score frame =
    return frame { _state = LastStrikeInProgress2 score }

rollForLastStrikeInProgress2 :: MonadReader Config reader => Int -> Int -> Frame -> reader Frame
rollForLastStrikeInProgress2 firstScore secondScore frame =
    return frame { _state = LastStrike (firstScore, secondScore) }
    
rollForLastSpareInProgress :: MonadReader Config reader => Int -> Int -> Frame -> reader Frame
rollForLastSpareInProgress firstScore secondScore frame =
    return frame { _state = LastSpare (firstScore, secondScore) }

rollFrame :: MonadReader Config reader => Int -> Frame -> reader (Either BowlingError Frame)
rollFrame score frame = do
    config <- ask
    if score >= 0 && score <= config^.numberOfPins then
        let rollFun = case frame^.state of
                        NotStarted ->
                            Right rollForNotStarted
                        InProgress firstScore ->
                            let totalScore = firstScore + score
                                in if totalScore <= config^.numberOfPins
                                then Right $ rollForInProgress firstScore
                                else Left $ InvalidScore totalScore
                        Open _ | frame^.number < config^.numberOfFrames ->
                            Right rollForFinished
                        Strike ->
                            Right rollForFinished
                        Spare _ ->
                            Right rollForFinished
                        LastStrikeInProgress1 ->
                            Right rollForLastStrikeInProgress1
                        LastStrikeInProgress2 firstScore ->
                            Right $ rollForLastStrikeInProgress2 firstScore
                        LastSpareInProgress firstScore ->
                            Right $ rollForLastSpareInProgress firstScore
                        _ ->
                            Left RollAfterLastFrame
         in return $ rollFun <&> \rollFun' -> runReader (rollFun' score frame) config
    else
        return $ Left $ InvalidScore score

getScores :: MonadReader Config reader => [Frame] -> reader [Maybe Int]
getScores frames = do
    numPins <- asks (view numberOfPins)
    return $
        frames >>= \frame ->
            case frame^.state of
                NotStarted ->
                    [ Nothing, Nothing ]
                InProgress score ->
                    [ Just score, Nothing ]
                Open (firstScore, secondScore) ->
                    [ Just firstScore, Just secondScore ]
                Strike ->
                    [ Just numPins, Nothing ]
                LastStrikeInProgress1 ->
                    [ Just numPins, Nothing, Nothing ]
                LastStrikeInProgress2 score ->
                    [ Just numPins, Just score, Nothing ]
                LastStrike (firstScore, secondScore) ->
                    [ Just numPins, Just firstScore, Just secondScore ]
                Spare score ->
                    [ Just score, Just (numPins - score) ]
                LastSpareInProgress score ->
                    [ Just score, Just (numPins - score), Nothing ]
                LastSpare (firstScore, secondScore) ->
                    [ Just firstScore, Just (numPins - firstScore), Just secondScore ]

getTotal :: [FrameScore] -> Int
getTotal = maybe 0 id . fmap (maybe 0 id . view totalScore) . headMay

getTotalScores :: MonadReader Config reader => [Frame] -> reader [FrameScore]
getTotalScores frames =
    getTotalScores' [] frames where
    getTotalScores' = \frameScores frames -> do
        numPins <- ask <&> view numberOfPins
        case frames of
            [] -> return $ reverse frameScores
            frame : otherFrames -> join . return $ do
                scores <- getScores otherFrames <&> catMaybes
                let total = getTotal frameScores
                let score = case frame^.state of
                                NotStarted ->
                                    frameScore
                                InProgress score ->
                                    frameScore {
                                        _total = Just $ total + score,
                                        _firstRoll = Just score
                                    }
                                Open (firstScore, secondScore) ->
                                    frameScore {
                                        _total = Just $ total + firstScore + secondScore,
                                        _firstRoll = Just firstScore,
                                        _secondRoll = Just secondScore
                                    }
                                Strike ->
                                    frameScore {
                                        _total = Just $ total + numPins + (foldl (+) 0 $ take 2 scores),
                                        _firstRoll = Just numPins
                                    }
                                Spare score ->
                                    frameScore {
                                        _total = Just $ total + numPins + (maybe 0 id $ headMay scores),
                                        _firstRoll = Just score,
                                        _secondRoll = Just $ numPins - score
                                    }
                                LastStrikeInProgress1 ->
                                    frameScore {
                                        _total = Just $ total + numPins,
                                        _firstRoll = Just numPins
                                    }
                                LastStrikeInProgress2 score ->
                                    frameScore {
                                        _total = Just $ total + numPins + score,
                                        _firstRoll = Just numPins,
                                        _secondRoll = Just score
                                    }
                                LastStrike (firstScore, secondScore) ->
                                    frameScore {
                                        _total = Just $ total + numPins + firstScore + secondScore,
                                        _firstRoll = Just numPins,
                                        _secondRoll = Just firstScore,
                                        _thirdRoll = Just secondScore
                                    }
                                LastSpareInProgress score ->
                                    frameScore {
                                        _total = Just $ total + numPins,
                                        _firstRoll = Just score,
                                        _secondRoll = Just $ numPins - score
                                    }
                                LastSpare (firstScore, secondScore) ->
                                    frameScore {
                                        _total = Just $ total + numPins + secondScore,
                                        _firstRoll = Just firstScore,
                                        _secondRoll = Just $ numPins - firstScore,
                                        _thirdRoll = Just secondScore
                                    }
                
                getTotalScores' (score : frameScores) otherFrames
