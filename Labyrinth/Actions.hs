module Labyrinth.Actions()
where

import Labyrinth.Helpers
import Labyrinth.Models
import Labyrinth.Factory
import qualified Labyrinth.Board

showBoard :: Game -> String
showBoard (Game _ _ board) = show board

showFreeTile :: Game -> String
showFreeTile (Game _ freeTile _) = show freeTile

showPawns :: Game -> String
showPawns (Game players _ _) = unlines $ map
  (\(Player color _ position _) -> show color ++ ": " ++ show position)
  players

showTreasureCards :: Game -> String
showTreasureCards (Game (Player _ _ _ cards:ps) _ _) = show cards

showRemainingTreasureCardsOfPlayer :: Player -> String
showRemainingTreasureCardsOfPlayer (Player color _ _ cards) = show color
  ++ ": "
  ++ (show . length) cards

showRemainingTreasureCards :: Game -> String
showRemainingTreasureCards (Game players _ _) = unlines
  $ map showRemainingTreasureCardsOfPlayer players

insertFreeTile :: Game -> Labyrinth.Board.InsertionPoint -> Direction -> Game
insertFreeTile (Game players freeTile board) insertionPoint direction = let
  tile = freeTileToTile freeTile direction
  (newBoard, newFreeTile) = Labyrinth.Board.insert board insertionPoint tile
  in Game players newFreeTile newBoard
