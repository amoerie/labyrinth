module Main where

import qualified Labyrinth.Helpers
import qualified Labyrinth.Models
import qualified Labyrinth.Game
import qualified Labyrinth.Factory
import qualified Labyrinth.Board
import qualified Labyrinth.Parser
import qualified Data.List
import qualified System.Random
import qualified System.Environment

createNewGame :: IO Labyrinth.Models.Game
createNewGame = do
  putStrLn "Hello and welcome to Labyrinth! How many players will be playing today?"
  numberOfPlayersAsString <- getLine
  let numberOfPlayers = read numberOfPlayersAsString :: Int
  generator <- System.Random.getStdGen
  return $ Labyrinth.Factory.createNewGame numberOfPlayers generator

openExistingGame :: String -> IO Labyrinth.Models.Game
openExistingGame gameFileName = do
  gameFileContents <- readFile gameFileName
  let game = (fst . head) $ Labyrinth.Parser.apply Labyrinth.Parser.labyrinth gameFileContents
  return game

startGame :: Labyrinth.Models.Game -> IO()
startGame game = do
  Labyrinth.Game.takeTurn game
  return ()

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    []     -> createNewGame >>= startGame
    [file] -> openExistingGame file >>= startGame
    _      -> putStrLn "Wrong number of arguments. Pass in zero arguments to start a new game, or pass in a file name to resume an existing game."
