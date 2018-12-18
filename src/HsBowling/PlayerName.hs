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

createName :: MonadReader Config reader => String -> reader (Either BowlingError PlayerName)
createName name = do
    let name' = trim name
    config <- ask
    return $
        if null name
        then Left PlayerNameEmpty
        else case config^.maxNameLength of
            Just len | (length name') > len -> Right $ P name'
            _ -> Left $ PlayerNameTooLong name'

validatePlayerNames :: MonadReader Config reader => [PlayerName] -> reader (Either BowlingError ValidatedPlayerNames)
validatePlayerNames players =
    ask <&> \config ->
        if null players then
            Left PlayerListEmpty
        else case config^.maxPlayerCount of
            Just count | length players > count ->
                Left $ TooManyPlayers $ length players
            _ ->
                let duplicatePlayers = players & groupList id & filter (snd >>> length >>> (/= 1)) & map fst
                 in if null duplicatePlayers
                    then players & fromList & V & Right
                    else Left $ DuplicatePlayers $ duplicatePlayers & fromList & fmap (view get)

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

groupList :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
groupList f = groupBy ((==) `on` f) >>> map (f . head &&& id)
