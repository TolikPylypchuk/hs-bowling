module Main where

import Control.Lens.Operators
import Control.Monad.Reader
import System.IO

import HsBowling.Config
import HsBowling.Game
import HsBowling.Output
import HsBowling.Input

createGame' :: ReaderT Config IO Game
createGame' = do
    players <- readPlayers
    game <- createGame players
    case game of
        Right game' ->
            return game'
        Left error -> do
            formattedError <- formatError error
            liftIO $ putStrLn formattedError
            liftIO $ putStrLn ""
            liftIO $ hFlush stdout
            createGame'

play :: Game -> ReaderT Config IO Int
play game = do
    isGameFinished <- isGameFinished game

    if isGameFinished then
        return 0
    else do
        score <- liftIO $ readRoll game
        newGame <- rollGame score game
        case newGame of
            Right newGame' -> do
                formattedGame <- formatGame newGame'
                liftIO $ putStrLn formattedGame
                liftIO $ hFlush stdout
                play newGame'
            Left error -> do
                formattedError <- formatError error
                liftIO $ putStrLn formattedError
                liftIO $ putStrLn ""
                liftIO $ hFlush stdout
                play game

main :: IO Int
main = do
    putStrLn "Welcome to HsBowling Console!"
    createGame' >>= play & flip runReaderT defaultConfig
