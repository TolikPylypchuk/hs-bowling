{-# LANGUAGE FlexibleContexts #-}

module HsBowling.Frame (
    FrameState, Frame, FrameScore, state, number, total, firstRoll, secondRoll, thirdRoll,
    create, isFinished, isLast, roll
) where

import Control.Lens.Getter
import Control.Lens.Operators
import Control.Monad.Reader

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

data Frame = F {
    _state :: FrameState,
    _number :: Int
}

data FrameScore = FS {
    _total :: Maybe Int,
    _firstRoll :: Maybe Int,
    _secondRoll :: Maybe Int,
    _thirdRoll :: Maybe Int
}

state :: Getter Frame FrameState
state = to _state

number :: Getter Frame Int
number = to _number

total :: Getter FrameScore (Maybe Int)
total = to _total

firstRoll :: Getter FrameScore (Maybe Int)
firstRoll = to _firstRoll

secondRoll :: Getter FrameScore (Maybe Int)
secondRoll = to _secondRoll

thirdRoll :: Getter FrameScore (Maybe Int)
thirdRoll = to _thirdRoll

frameScore :: FrameScore
frameScore = FS {
    _total = Nothing,
    _firstRoll = Nothing,
    _secondRoll = Nothing,
    _thirdRoll = Nothing
}

create :: MonadReader Config reader => Int -> reader (Either BowlingError Frame)
create num =
    ask <&> \config ->
        if num > 0 && num <= config^.numberOfFrames
        then Right $ F { _state = NotStarted, _number = num }
        else Left $ InvalidFrameNumber num

isFinished :: FrameState -> Bool
isFinished frame =
    case frame of
        Open _       -> True
        Strike       -> True
        LastStrike _ -> True
        Spare _      -> True
        LastSpare _  -> True
        _            -> False

isLast :: MonadReader Config reader => Frame -> reader Bool
isLast frame =
    ask <&> (view numberOfFrames) <&> \numFrames -> frame^.number == numFrames

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

roll :: MonadReader Config reader => Int -> Frame -> reader (Either BowlingError Frame)
roll score frame = do
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
