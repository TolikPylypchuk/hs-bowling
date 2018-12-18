{-# LANGUAGE FlexibleContexts #-}

module HsBowling.Player (
    Player, name, frames, createPlayer, rollPlayer, lastFrame, isPlayerFinished
) where

import Control.Arrow ((>>>))
import Control.Lens.Getter
import Control.Lens.Operators
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty, toList, fromList)

import HsBowling.Error
import HsBowling.Config
import HsBowling.Frame
import HsBowling.PlayerName

data Player = P {
    _name :: PlayerName,
    _frames :: NonEmpty Frame
}

name :: Getter Player PlayerName
name = to _name

frames :: Getter Player (NonEmpty Frame)
frames = to _frames

createPlayer :: MonadReader Config reader => PlayerName -> reader (Either BowlingError Player)
createPlayer name =
    createFrame 1 <&> fmap (\frame -> P {
        _name = name,
        _frames = fromList [ frame ]
    })

rollPlayer :: MonadReader Config reader => Int -> Player -> reader (Either BowlingError Player)
rollPlayer score player =
    ask <&> \config -> do
        let reversedFrames = player^.frames & toList & reverse
        
        frame <- runReader (rollFrame score $ head reversedFrames) config

        let frames = frame : (tail reversedFrames)
        let number' = frame^.number

        result <-
            if isFrameFinished frame && number' /= config^.numberOfFrames then
                let result = runReader (createFrame $ number' + 1) config
                 in result <&> \frame' -> frame' : frames
            else
                Right frames
        
        return $ player { _frames = fromList result }

lastFrame :: Player -> Frame
lastFrame = view frames >>> toList >>> last

isPlayerFinished :: MonadReader Config reader => Player -> reader Bool
isPlayerFinished player =
    ask <&> \config ->
        let frame = lastFrame player
         in frame^.number == config^.numberOfFrames && isFrameFinished frame
