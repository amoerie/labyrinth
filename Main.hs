module Main where

import qualified Labyrinth.Helpers
import qualified Labyrinth.Models
import qualified Labyrinth.Factory
import qualified Labyrinth.Board
import qualified Labyrinth.Actions
import qualified Data.List
import qualified System.Random
import qualified System.Process

createNewGame :: Int -> IO Labyrinth.Models.Game
createNewGame numberOfPlayers = do
  stdGen <- System.Random.getStdGen
  let game = Labyrinth.Factory.createNewGame numberOfPlayers stdGen
  return game

takeTurn :: Labyrinth.Models.Game -> IO Labyrinth.Models.Game
takeTurn game = do
  System.Process.callCommand "cls"
  putStrLn $ Labyrinth.Actions.showBoard game
  putStrLn $ "Your cards      : " ++ Labyrinth.Actions.showTreasureCards game
  putStrLn "All pawns       : "
  putStrLn $ Labyrinth.Actions.showPawns game
  putStrLn "Remaining cards : "
  putStrLn $ Labyrinth.Actions.showRemainingTreasureCards game
  putStrLn "Free tile       : "
  putStrLn $ Labyrinth.Actions.showFreeTile game
  putStrLn "Where should the the free tile be inserted?"
  putStrLn $ "Options: [" ++ Data.List.intercalate ", " Labyrinth.Board.insertionPointIdentifiers ++ "]"
  insertionPointIdentifier <- getLine
  let insertionPoint = Labyrinth.Board.identifierToInsertionPoint insertionPointIdentifier
  putStrLn "How should the free tile be oriented?"
  putStrLn "Options: [North, East, South, West]"
  directionAsString <- getLine
  let direction = read directionAsString :: Labyrinth.Models.Direction
  let gameAfterInsert = Labyrinth.Actions.insertFreeTile insertionPoint direction game
  putStrLn "The free tile was inserted! This is what the board looks like now: "
  putStrLn $ Labyrinth.Actions.showBoard gameAfterInsert
  putStrLn "These treasures are now reachable from your current position: "
  putStrLn $ Labyrinth.Actions.showReachableTreasures gameAfterInsert
  putStrLn "Where do you want to move your pawn? (0,0) .. (6,6)"
  positionAsString <- getLine
  let position = read positionAsString :: Labyrinth.Models.Position
  let gameAfterMove = Labyrinth.Actions.movePawn position gameAfterInsert
  putStrLn $ "Remaining cards : " ++ Labyrinth.Actions.showRemainingTreasureCards gameAfterMove
  takeTurn gameAfterMove

main :: IO Labyrinth.Models.Game
main = do
  putStrLn "Hello and welcome to Labyrinth! How many players will be playing today?"
  numberOfPlayersAsString <- getLine
  let numberOfPlayers = read numberOfPlayersAsString :: Int
  game <- createNewGame numberOfPlayers
  takeTurn game
