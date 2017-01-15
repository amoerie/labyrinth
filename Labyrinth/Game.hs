module Labyrinth.Game (takeTurn)
where

import Control.Applicative
import qualified Data.Maybe
import qualified Data.List
import qualified Data.Ord
import qualified System.Process

import Labyrinth.Models
import qualified Labyrinth.Board
import qualified Labyrinth.Factory
import qualified Labyrinth.Writer

getCurrentPlayer :: Game -> Player
getCurrentPlayer (Game (p:ps) _ _) = p

getPlayers :: Game -> [Player]
getPlayers (Game players _ _) = players

getBoard :: Game -> Board
getBoard (Game _ _ board) = board

getFreeTile :: Game -> FreeTile
getFreeTile (Game _ freeTile _) = freeTile

getColor :: Player -> Color
getColor (Player color _ _ _) = color

getControl :: Player -> Control
getControl (Player _ control _ _) = control

getCards :: Player -> Cards
getCards (Player _ _ _ cards) = cards

getPawnPosition :: Player -> Position
getPawnPosition (Player _ _ position _) = position

isWinner :: Player -> Bool
isWinner (Player _ _ _ []) = True
isWinner _                 = False

getWinner :: Game -> Maybe Player
getWinner (Game players _ _) = Data.List.find isWinner players

getPawnPositions :: Game -> [Position]
getPawnPositions (Game players _ _) = map getPawnPosition players

showBoard :: Game -> String
showBoard = show . getBoard

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

-- Moves the player pawn if it affected by inserting the free tile in the insertion position
movePawnIfNecessary :: Labyrinth.Board.InsertionPoint -> Player -> Player
movePawnIfNecessary (Labyrinth.Board.InsertionPoint pos dir) player
  | pawnPosition `elem` affectedPositions = movePlayerPawn player
  | otherwise                             = player
  where affectedPositions = Labyrinth.Board.getAffectedPositions (Labyrinth.Board.InsertionPoint pos dir)
        associatedMove = Labyrinth.Board.getAssociatedMove dir
        pawnPosition = getPawnPosition player
        newPawnPosition = associatedMove pawnPosition
        movePlayerPawn (Player clr ctrl _ cards) = Player clr ctrl newPawnPosition cards

insertFreeTile :: Labyrinth.Board.InsertionPoint -> Direction -> Game -> Game
insertFreeTile insertionPoint direction game = Game players newFreeTile newBoard
  where tile = Labyrinth.Factory.freeTileToTile (getFreeTile game) direction
        (newBoard, newFreeTile) = Labyrinth.Board.insert (getBoard game) insertionPoint tile
        players = map (movePawnIfNecessary insertionPoint) (getPlayers game)

doMovePawn :: Position -> Game -> Game
doMovePawn position (Game (Player clr ctrl pos crds:ps) freeTile board) = Game (ps ++ [player]) freeTile boardWithoutTreasure
  where (Tile _ treasure _) = Labyrinth.Board.getTile position board
        remainingCards = if Data.Maybe.isJust treasure then filter (Data.Maybe.fromJust treasure /=) crds else crds
        player = Player clr ctrl position remainingCards
        boardWithoutTreasure = Labyrinth.Board.removeTreasure treasure board

movePawn :: Position -> Game -> Game
movePawn position (Game (Player clr ctrl pos crds:ps) freeTile board)
  | (not . Labyrinth.Board.isValid) position = error "Not a valid position"
  | position `notElem` reachablePositions    = error "Not reachable from the current position"
  | otherwise                                = doMovePawn position (Game (Player clr ctrl pos crds:ps) freeTile board)
  where reachablePositions = Labyrinth.Board.getReachablePositions pos board

getAllTreasures :: Game -> [(Treasure, Position)]
getAllTreasures = Labyrinth.Board.getAllTreasures . getBoard

getPlayerTreasures :: Game -> [(Treasure, Position)]
getPlayerTreasures game = filter ((`elem` cardsAsTreasures) . fst) allTreasures
  where currentPlayer    = getCurrentPlayer game
        cards            = getCards currentPlayer
        cardsAsTreasures = map Data.Maybe.Just cards
        allTreasures     = getAllTreasures game

getReachableTreasures :: Game -> [(Treasure, Position)]
getReachableTreasures (Game (Player _ _ position cards:ps) _ board) = filter ((`elem` cardsAsTreasures) . fst) reachableTreasures
  where reachableTreasures = Labyrinth.Board.getReachableTreasures position board
        cardsAsTreasures = map Data.Maybe.Just cards

showReachableTreasures :: Game -> String
showReachableTreasures game = Data.List.intercalate ", "
  $ map (\(t, p) -> ((show . treasureToChar) t ++ show p))
  $ getReachableTreasures game

showReachablePositions :: Game -> String
showReachablePositions game = Data.List.intercalate " or "
  $ map show
  $ Labyrinth.Board.getReachablePositions position board
  where position = (getPawnPosition . getCurrentPlayer) game
        board = getBoard game

-- An insertion point is only allowed when, upon inserting the free tile, no pawns fall off the board
getAllowedInsertionPoints :: Game -> [Labyrinth.Board.InsertionPoint]
getAllowedInsertionPoints game = map fst
  $ filter ((`notElem` pawnPositions) . snd) insertionPointsWithLastAffectedPosition
  where pawnPositions = getPawnPositions game
        insertionPoints = Labyrinth.Board.insertionPoints
        lastAffectedPositions = map (Data.List.last . Labyrinth.Board.getAffectedPositions) insertionPoints
        insertionPointsWithLastAffectedPosition = Data.List.zip insertionPoints lastAffectedPositions

getAllowedInsertionPointIdentifiers :: Game -> [String]
getAllowedInsertionPointIdentifiers game = map snd
  $ filter ((`elem` allowedInsertionPoints) . fst) insertionPointsWithIdentifiers
  where identifiers = Labyrinth.Board.insertionPointIdentifiers
        insertionPoints = map Labyrinth.Board.identifierToInsertionPoint identifiers
        insertionPointsWithIdentifiers = zip insertionPoints identifiers
        allowedInsertionPoints = getAllowedInsertionPoints game

-- Represents a possible decision made by the AI: inserting the tile into the insertion point would return this game
data PossibleNextGame = PossibleNextGame Labyrinth.Board.InsertionPoint Direction Game

simulatePossibleNextGame :: Game -> Labyrinth.Board.InsertionPoint -> Direction -> PossibleNextGame
simulatePossibleNextGame game insertionPoint direction = PossibleNextGame insertionPoint direction newGame
  where newGame = insertFreeTile insertionPoint direction game

simulatePossibleNextGames :: Game -> [PossibleNextGame]
simulatePossibleNextGames game = simulatePossibleNextGame game <$> getAllowedInsertionPoints game <*> [North ..]

-- Determines the best possible position to move the current player's pawn towards
-- The int signifies the distance to the closest treasure
getBestReachablePosition :: Game -> (Position, Int)
getBestReachablePosition game = Data.List.minimumBy (Data.Ord.comparing snd)
  $ posWithDistance <$> reachablePositions <*> treasurePositions
  where player             = getCurrentPlayer game
        pawnPosition       = getPawnPosition player
        board              = getBoard game
        reachablePositions = Labyrinth.Board.getReachablePositions pawnPosition board
        treasurePositions  = map snd $ getPlayerTreasures game
        posWithDistance pos treasurePos = (pos, Labyrinth.Board.distanceBetween pos treasurePos)

-- Determines the best possible next game by trying all possible insertion points with all possible free tile orientations
-- If a treasure can be collected by any of these attempts, that move will win
-- Otherwise, we try to get as close as possible to any remaining treasure
-- Since the board is very unpredictable, it seems meaningless to think multiple steps ahead
getBestPossibleNextGame :: Game -> PossibleNextGame
getBestPossibleNextGame game = Data.List.minimumBy compareByBestReachablePosition
  $ simulatePossibleNextGames game
  where compareByBestReachablePosition = Data.Ord.comparing (\(PossibleNextGame i d game) -> (snd . getBestReachablePosition) game)

takeAITurn :: Game -> IO()
takeAITurn game = do
  System.Process.callCommand "cls"
  putStrLn $ "AI is thinking : " ++ (show . getColor . getCurrentPlayer) game
  let (PossibleNextGame insertionPoint direction _) = getBestPossibleNextGame game
  let gameAfterInsert = insertFreeTile insertionPoint direction game
  let (bestReachablePosition, _) = getBestReachablePosition gameAfterInsert
  let gameAfterMove = movePawn bestReachablePosition gameAfterInsert
  putStrLn "AI has finished his turn. The board now looks like this: "
  putStrLn $ showBoard gameAfterMove
  takeTurn gameAfterMove

humanTurnSaveAndExit :: Game -> IO()
humanTurnSaveAndExit game = do
  let serialized = Labyrinth.Writer.writeLabyrinth game
  writeFile "game.txt" serialized
  return ()

humanTurnKeepPlaying :: String -> Game -> IO()
humanTurnKeepPlaying insertionPointIdentifier game = do
  let insertionPoint = Labyrinth.Board.identifierToInsertionPoint insertionPointIdentifier
  putStrLn "How should the free tile be oriented?"
  putStrLn "Options: [North, East, South, West]"
  directionAsString <- getLine
  let direction = read directionAsString :: Direction
  let gameAfterInsert = insertFreeTile insertionPoint direction game
  putStrLn "The free tile was inserted! This is what the board looks like now: "
  putStrLn $ showBoard gameAfterInsert
  putStrLn "These treasures are now reachable from your current position: "
  putStrLn $ showReachableTreasures gameAfterInsert
  putStrLn "Where do you want to move your pawn?"
  putStrLn $ "Options: " ++ showReachablePositions gameAfterInsert
  positionAsString <- getLine
  let position = read positionAsString :: Position
  let gameAfterMove = movePawn position gameAfterInsert
  putStrLn $ "Remaining cards : " ++ showRemainingTreasureCards gameAfterMove
  takeTurn gameAfterMove

takeHumanTurn :: Game -> IO()
takeHumanTurn game = do
  System.Process.callCommand "cls"
  putStrLn $ showBoard game
  putStrLn $ "Your color      : " ++ (show . getColor . getCurrentPlayer) game
  putStrLn $ "Your cards      : " ++ showTreasureCards game
  putStrLn "All pawns       : "
  putStrLn $ showPawns game
  putStrLn "Remaining cards : "
  putStrLn $ showRemainingTreasureCards game
  putStrLn "Free tile       : "
  putStrLn $ showFreeTile game
  putStrLn "Where should the the free tile be inserted?"
  putStrLn $ "Options: [" ++ Data.List.intercalate ", " (getAllowedInsertionPointIdentifiers game) ++ "]"
  putStrLn "Optionally, type SAVE to save the game and quit"
  command <- getLine
  if command == "SAVE"
  then humanTurnSaveAndExit game
  else humanTurnKeepPlaying command game

showWinner :: Player -> IO()
showWinner (Player color control _ _) = do
  putStrLn $ show control ++ " player '" ++ show color ++ "' has won!"
  return ()

takeTurn :: Game -> IO()
takeTurn game
  | Data.Maybe.isJust winner = showWinner $ Data.Maybe.fromJust winner
  | control == Human         = takeHumanTurn game
  | control == AI            = takeAITurn    game
  where control = (getControl . getCurrentPlayer) game
        winner  = getWinner game
