module Labyrinth.Actions()
where

import Labyrinth.Models

showBoard :: Game -> Player -> String
showBoard (Game _ _ board) _ = show board

showFreeTile :: Game -> Player -> String
showFreeTile (Game _ freeTile _) _ = show freeTile

showPawns :: Game -> Player -> String
showPawns (Game players _ _) _ = unlines $ map
  (\(Player color _ position _) -> show color ++ show position)
  players

showTreasureCards :: Game -> Player -> String
showTreasureCards _ (Player _ _ _ cards) = show cards

showRemainingTreasureCardsOfPlayer :: Player -> String
showRemainingTreasureCardsOfPlayer (Player _ _ _ cards) = (show . length) cards

showRemainingTreasureCards :: Game -> Player -> String
showRemainingTreasureCards (Game players _ _) player = unlines
  $ map showRemainingTreasureCardsOfPlayer
  $ filter (/= player) players
