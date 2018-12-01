module HsBowling.Error where

import Data.List.NonEmpty

data ConfigError = InvalidNumberOfPins Int
                 | InvalidNumberOfFrames Int
                 | InvalidMaxNameLength Int
                 | InvalidMaxPlayerCount Int
                 deriving (Show, Eq)

data BowlingError = InvalidFrameNumber Int
                  | PlayerNameEmpty
                  | PlayerNameTooLong String
                  | PlayerListEmpty
                  | TooManyPlayers Int
                  | DuplicatePlayers (NonEmpty String)
                  | InvalidScore Int
                  | RollAfterLastFrame
                  deriving (Show, Eq)
