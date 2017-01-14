module Labyrinth.Actions(
  showBoard,
  showFreeTile,
  showPawns,
  showTreasureCards,
  showRemainingTreasureCards,
  showReachableTreasures,
  insertFreeTile,
  movePawn

)
where

import Labyrinth.Helpers
import Labyrinth.Models
import Labyrinth.Factory
import qualified Data.List
import qualified Labyrinth.Board
import qualified Data.Maybe

showBoard :: Game -> String
showBoard (Game _ _ board) = show board

showFreeTile :: Game -> String
showFreeTile (Game _ freeTile _) = show freeTile

showPawns :: Game -> String
showPawns (Game players _ _) = unlines $ map
  (\(Player color _ position _) -> show color ++ ": " ++ show position)
  players

showTreasureCards :: Game -> String
showTreasureCards (Game (Player _ _ _ cards:ps) _ _) = Data.List.intercalate ", "
  $ map (show . treasureToChar . intToTreasure) cards

showRemainingTreasureCardsOfPlayer :: Player -> String
showRemainingTreasureCardsOfPlayer (Player color _ _ cards) = show color
  ++ ": "
  ++ (show . length) cards

showRemainingTreasureCards :: Game -> String
showRemainingTreasureCards (Game players _ _) = unlines
  $ map showRemainingTreasureCardsOfPlayer players

insertFreeTile :: Labyrinth.Board.InsertionPoint -> Direction -> Game -> Game
insertFreeTile insertionPoint direction (Game players freeTile board)  = let
  tile = freeTileToTile freeTile direction
  (newBoard, newFreeTile) = Labyrinth.Board.insert board insertionPoint tile
  in Game players newFreeTile newBoard

doMovePawn :: Position -> Game -> Game
doMovePawn position (Game (Player clr ctrl pos crds:ps) freeTile board) = Game (ps ++ [player]) freeTile board
  where (Tile _ treasure _) = Labyrinth.Board.getTile position board
        remainingCards = if Data.Maybe.isJust treasure then filter (Data.Maybe.fromJust treasure ==) crds else crds
        player = Player clr ctrl position remainingCards

movePawn :: Position -> Game -> Game
movePawn position (Game (Player clr ctrl pos crds:ps) freeTile board)
  | (not . Labyrinth.Board.isValid) position = error "Not a valid position"
  | position `notElem` reachablePositions    = error "Not reachable from the current position"
  | otherwise                                = doMovePawn position (Game (Player clr ctrl pos crds:ps) freeTile board)
  where reachablePositions = Labyrinth.Board.getReachablePositions pos board

showReachableTreasures :: Game -> String
showReachableTreasures (Game (Player _ _ position cards:ps) _ board) = Data.List.intercalate ", "
  $ map (show . treasureToChar)
  $ filter (`elem` cardsAsTreasures) reachableTreasures
  where reachableTreasures = Labyrinth.Board.getReachableTreasures position board
        cardsAsTreasures = map Data.Maybe.Just cards
