module Main where

import qualified Labyrinth.Helpers
import qualified Labyrinth.Models
import qualified Labyrinth.Game
import qualified Labyrinth.Factory
import qualified Labyrinth.Board
import qualified Data.List
import qualified System.Random

createNewGame :: Int -> IO Labyrinth.Models.Game
createNewGame numberOfPlayers = do
  stdGen <- System.Random.getStdGen
  let game = Labyrinth.Factory.createNewGame numberOfPlayers stdGen
  return game

main :: IO ()
main = do
  putStrLn "Hello and welcome to Labyrinth! How many players will be playing today?"
  numberOfPlayersAsString <- getLine
  let numberOfPlayers = read numberOfPlayersAsString :: Int
  game <- createNewGame numberOfPlayers
  Labyrinth.Game.takeTurn game
