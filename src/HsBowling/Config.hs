module HsBowling.Config (
    Config, numberOfPins, numberOfFrames, maxNameLength, maxPlayerCount, defaultConfig, create
) where

import Data.Validation
import HsBowling.Error

data Config = C {
    numberOfPins' :: Integer,
    numberOfFrames' :: Integer,
    maxNameLength' :: Maybe Integer,
    maxPlayerCount' :: Maybe Integer
} deriving Show

numberOfPins :: Config -> Integer
numberOfPins = numberOfPins'

numberOfFrames :: Config -> Integer
numberOfFrames = numberOfFrames'

maxNameLength :: Config -> Maybe Integer
maxNameLength =  maxNameLength'

maxPlayerCount :: Config -> Maybe Integer
maxPlayerCount = maxPlayerCount'

defaultConfig :: Config
defaultConfig = C {
    numberOfPins'   = 10,
    numberOfFrames' = 10,
    maxNameLength'  = Nothing,
    maxPlayerCount' = Just 6
}

positiveOrElse :: (Integer -> ConfigError) -> Integer -> Validation [ConfigError] Integer
positiveOrElse error num =
    if num > 0 then Success num else Failure [ error num ]

forMaybe :: (Integer -> Validation [ConfigError] Integer) -> Maybe Integer -> Validation [ConfigError] (Maybe Integer)
forMaybe validationFunc =
    sequenceA . fmap validationFunc

validateNumPins :: Integer -> Validation [ConfigError] Integer
validateNumPins = positiveOrElse InvalidNumberOfPins

validateNumFrames :: Integer -> Validation [ConfigError] Integer
validateNumFrames = positiveOrElse InvalidNumberOfFrames

validateMaxNameLength :: Maybe Integer -> Validation [ConfigError] (Maybe Integer)
validateMaxNameLength = forMaybe $ positiveOrElse InvalidMaxNameLength

validateMaxPlayerCount :: Maybe Integer -> Validation [ConfigError] (Maybe Integer)
validateMaxPlayerCount = forMaybe $ positiveOrElse InvalidMaxPlayerCount

createConfig :: Integer -> Integer -> Maybe Integer -> Maybe Integer -> Config
createConfig numPins numFrames maxNameLength maxPlayerCount = C {
    numberOfPins'   = numPins,
    numberOfFrames' = numFrames,
    maxNameLength'  = maxNameLength,
    maxPlayerCount' = maxPlayerCount
}

create :: Integer -> Integer -> Maybe Integer -> Maybe Integer -> Validation [ConfigError] Config
create numPins numFrames maxNameLength maxPlayerCount =
    let numPins'        = validateNumPins numPins
        numFrames'      = validateNumFrames numFrames
        maxNameLength'  = validateMaxNameLength maxNameLength
        maxPlayerCount' = validateMaxPlayerCount maxPlayerCount
     in createConfig <$> numPins' <*> numFrames' <*> maxNameLength' <*> maxPlayerCount'
