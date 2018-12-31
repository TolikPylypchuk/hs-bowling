{-# LANGUAGE FlexibleContexts #-}

module HsBowling.PlayerName (
    PlayerName, ValidatedPlayerNames, get, getNames, createName, validatePlayerNames
) where

import Control.Arrow ((&&&), (>>>))
import Control.Lens.Getter
import Control.Lens.Operators
import Control.Monad.Reader
import Data.Char (isSpace)
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty, fromList)

import HsBowling.Util
import HsBowling.Error
import HsBowling.Config

data PlayerName = P String
                  deriving (Show, Eq, Ord)

data ValidatedPlayerNames = V (NonEmpty PlayerName)
                            deriving (Show, Eq)

get :: Getter PlayerName String
get = to $ \(P name) -> name

getNames :: Getter ValidatedPlayerNames (NonEmpty PlayerName)
getNames = to $ \(V names) -> names

createName :: String -> ReaderT Config (Either BowlingError) PlayerName
createName name = do
    let name' = trim name
    maxNameLength' <- asks $ view maxNameLength
    if null name
    then returnError PlayerNameEmpty
    else case maxNameLength' of
        Just maxLen | (length name') > maxLen -> returnError $ PlayerNameTooLong name'
        _ -> return $ P name'

validatePlayerNames :: [PlayerName] -> ReaderT Config (Either BowlingError) ValidatedPlayerNames
validatePlayerNames players =
    ask >>= \config ->
        if null players then
            returnError PlayerListEmpty
        else case config^.maxPlayerCount of
            Just count | length players > count ->
                returnError $ TooManyPlayers $ length players
            _ ->
                let duplicatePlayers = players & groupList id & filter (snd >>> length >>> (/= 1)) & map fst
                 in if null duplicatePlayers
                    then return $ V $ fromList players
                    else returnError $ DuplicatePlayers $ duplicatePlayers & fromList & fmap (view get)

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

groupList :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
groupList f = groupBy ((==) `on` f) >>> map (f . head &&& id)
