module HsBowling.Error where

import Data.List.NonEmpty

data ConfigError = InvalidNumberOfPins Integer
                 | InvalidNumberOfFrames Integer
                 | InvalidMaxNameLength Integer
                 | InvalidMaxPlayerCount Integer

data BowlingError = InvalidFrameNumber Integer
                  | PlayerNameEmpty
                  | PlayerNameTooLong String
                  | PlayerListEmpty
                  | TooManyPlayers Integer
                  | DuplicatePlayers (NonEmpty String)
                  | InvalidScore Integer
                  | RollAfterLastFrame
