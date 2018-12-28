module HsBowling.Input (
    readNumPlayers, readPlayer, readPlayers, readRoll
) where

import Control.Lens.Getter
import Control.Lens.Operators
import Control.Monad.Reader
import System.IO
import Text.Read (readMaybe)

import HsBowling.Config
import HsBowling.PlayerName
import HsBowling.Player
import HsBowling.Game
import HsBowling.Output

getInt :: IO (Maybe Int)
getInt = getLine >>= (return . readMaybe)

readNumPlayers :: ReaderT Config IO Int
readNumPlayers = do
    liftIO $ putStr "Enter the number of players: "
    liftIO $ hFlush stdout
    readNumPlayers'
    where readNumPlayers' = do
            maxPlayerCount' <- asks $ view maxPlayerCount
            numPlayers <- liftIO $ getInt
            liftIO $ putStrLn $ show numPlayers
            case (numPlayers, maxPlayerCount') of
                (Just num, Nothing) | num > 0 ->
                    return num
                (Just num, Just count) | num > 0 && num <= count ->
                    return num
                (_, Just count) -> do
                    liftIO $ putStr $ "\nThe number of players must be positive and at most " ++ show count ++ ". Please try again: "
                    liftIO $ hFlush stdout
                    readNumPlayers'
                (_, Nothing) -> do
                    liftIO $ putStr "\nThe number of players must be positive. Please try again: "
                    liftIO $ hFlush stdout
                    readNumPlayers'

readPlayer :: Int -> ReaderT Config IO PlayerName
readPlayer index = do
    liftIO $ putStr $ "\nEnter the name of player #" ++ show index ++ ": "
    liftIO $ hFlush stdout
    readPlayer'
    where readPlayer' = do
            playerName <- liftIO getLine
            name <- createName playerName
            case name of
                Right name' ->
                    return name'
                Left error -> do
                    formattedError <- error & formatError
                    liftIO $ putStrLn formattedError
                    liftIO $ putStr "Please try again: "
                    liftIO $ hFlush stdout
                    readPlayer'

readPlayers :: ReaderT Config IO ValidatedPlayerNames
readPlayers = do
    numPlayers <- readNumPlayers
    names <- sequenceA [ readPlayer i | i <- [1 .. numPlayers] ] 
    validatedNames <- validatePlayerNames names

    case validatedNames of
        Right names -> do
            liftIO $ putStrLn ""
            return names
        Left error -> do
            liftIO $ putStrLn "\nThe player list is invalid."
            liftIO $ hFlush stdout
            formattedError <- error & formatError
            liftIO $ putStrLn formattedError
            liftIO $ putStrLn "nPlease try again.\n"
            liftIO $ hFlush stdout
            readPlayers

readRoll :: Game -> IO Int
readRoll game = do
    let currentPlayer' = (currentPlayer game)^.name^.get
    putStr $ currentPlayer' ++ " rolls with score: "
    hFlush stdout
    readRoll'
    where readRoll' = do
            score <- getInt
            case score of
                Just score' | score' >= 0 -> do
                    putStrLn ""
                    hFlush stdout
                    return score'
                Nothing -> do
                    putStr "The score must be a non-negative number. Please try again: "
                    hFlush stdout
                    readRoll'
