{-# LANGUAGE TemplateHaskell #-}

module HsBowling.Config (
    Config, numberOfPins, numberOfFrames, maxNameLength, maxPlayerCount, defaultConfig, createConfig
) where

import Control.Lens
import Control.Lens.Getter
import Data.Validation
import HsBowling.Error

data Config = C {
    _numberOfPins :: Int,
    _numberOfFrames :: Int,
    _maxNameLength :: Maybe Int,
    _maxPlayerCount :: Maybe Int
} deriving (Show, Eq)

numberOfPins :: Getter Config Int
numberOfPins = to _numberOfPins

numberOfFrames :: Getter Config Int
numberOfFrames = to _numberOfFrames

maxNameLength :: Getter Config (Maybe Int)
maxNameLength = to _maxNameLength

maxPlayerCount :: Getter Config (Maybe Int)
maxPlayerCount = to _maxPlayerCount

defaultConfig :: Config
defaultConfig = C {
    _numberOfPins   = 10,
    _numberOfFrames = 10,
    _maxNameLength  = Nothing,
    _maxPlayerCount = Just 6
}

positiveOrElse :: (Int -> ConfigError) -> Int -> Validation [ConfigError] Int
positiveOrElse error num =
    if num > 0 then Success num else Failure [ error num ]

forMaybe :: (Int -> Validation [ConfigError] Int) -> Maybe Int -> Validation [ConfigError] (Maybe Int)
forMaybe validationFunc =
    sequenceA . fmap validationFunc

validateNumPins :: Int -> Validation [ConfigError] Int
validateNumPins = positiveOrElse InvalidNumberOfPins

validateNumFrames :: Int -> Validation [ConfigError] Int
validateNumFrames = positiveOrElse InvalidNumberOfFrames

validateMaxNameLength :: Maybe Int -> Validation [ConfigError] (Maybe Int)
validateMaxNameLength = forMaybe $ positiveOrElse InvalidMaxNameLength

validateMaxPlayerCount :: Maybe Int -> Validation [ConfigError] (Maybe Int)
validateMaxPlayerCount = forMaybe $ positiveOrElse InvalidMaxPlayerCount

createConfig' :: Int -> Int -> Maybe Int -> Maybe Int -> Config
createConfig' numPins numFrames maxNameLength maxPlayerCount = C {
    _numberOfPins   = numPins,
    _numberOfFrames = numFrames,
    _maxNameLength  = maxNameLength,
    _maxPlayerCount = maxPlayerCount
}

createConfig :: Int -> Int -> Maybe Int -> Maybe Int -> Validation [ConfigError] Config
createConfig numPins numFrames maxNameLength maxPlayerCount =
    let numPins'        = validateNumPins numPins
        numFrames'      = validateNumFrames numFrames
        maxNameLength'  = validateMaxNameLength maxNameLength
        maxPlayerCount' = validateMaxPlayerCount maxPlayerCount
     in createConfig' <$> numPins' <*> numFrames' <*> maxNameLength' <*> maxPlayerCount'
