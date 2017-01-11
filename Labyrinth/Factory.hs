module Labyrinth.Factory (
  createNewBoard,
  allColors,
  allTreasures,
  allStartingPositions,
  isStartingPosition,
  positionToIndex,
  indexToPosition,
  tileToFreeTile,
  freeTileToTile
)
where

{-|
  The factory is responsible for starting a completely new game.
  It draws a new board and distributes the tiles and treasures
-}

import Labyrinth.Models
import Labyrinth.Helpers

import Control.Arrow (second)
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Random

fixedTiles :: [(Tile, Position)]
fixedTiles = [
    (Tile Corner Nothing East, (0,0)),
    (Tile TShape Nothing South,(0,2)),
    (Tile TShape Nothing South,(0,4)),
    (Tile Corner Nothing South,(0,6)),

    (Tile TShape Nothing East, (2,0)),
    (Tile TShape Nothing East, (2,2)),
    (Tile TShape Nothing South,(2,4)),
    (Tile TShape Nothing West, (2,6)),

    (Tile TShape Nothing East, (4,0)),
    (Tile TShape Nothing North,(4,2)),
    (Tile TShape Nothing West, (4,4)),
    (Tile TShape Nothing West, (4,6)),

    (Tile Corner Nothing North,(6,0)),
    (Tile TShape Nothing North,(6,2)),
    (Tile TShape Nothing North,(6,4)),
    (Tile Corner Nothing West, (6,6))
  ]

colorToStartingPosition :: Color -> Position
colorToStartingPosition Yellow = (0,0)
colorToStartingPosition Red    = (0,6)
colorToStartingPosition Green  = (6,0)
colorToStartingPosition Blue   = (6,6)

allColors :: [Color]
allColors = [(minBound :: Color ) .. ]

allTreasures :: [Int]
allTreasures = [0 .. 23]

allStartingPositions :: [Position]
allStartingPositions = map colorToStartingPosition allColors

isStartingPosition :: Position -> Bool
isStartingPosition pos = pos `elem` allStartingPositions

positionToIndex :: Position -> Int
positionToIndex (x,y) = x * labyrinthSize + y

indexToPosition :: Int -> Position
indexToPosition index = (index `div` labyrinthSize, index `mod` labyrinthSize)

tileToFreeTile :: Tile -> FreeTile
tileToFreeTile (Tile kind treasure _) = FreeTile kind treasure

freeTileToTile :: FreeTile -> Direction -> Tile
freeTileToTile (FreeTile kind treasure) = Tile kind treasure

type FreeTiles = [FreeTile]
poolOfFreeTiles :: FreeTiles
poolOfFreeTiles = replicate 16 (FreeTile Corner Nothing)
  ++ replicate 6 (FreeTile TShape Nothing)
  ++ replicate 12 (FreeTile Line Nothing)

type FreePositions = [Position]
poolOfFreePositions :: FreePositions
poolOfFreePositions = map indexToPosition
  $ filter (`notElem` fixedIndex) [0..(labyrinthSize * labyrinthSize - 1)]
  where fixedIndex = map (positionToIndex . snd) fixedTiles

{-|
 Takes a list of free tiles and returns a list of tiles with random directions
-}
createTiles :: [FreeTile] -> StdGen -> ([Tile], StdGen)
createTiles []        generator = ([], generator)
createTiles freeTiles generator = let (randomDirection, nextGenerator) = random generator
                                      (FreeTile kind treasure:ts) = freeTiles
                                      (tiles, lastGenerator) = createTiles ts nextGenerator
                                  in  (Tile kind treasure randomDirection : tiles, lastGenerator)

{-|
 Takes a list of free tiles with their assigned positions and returns a fully complete board
-}
createBoard :: [(Tile,Position)] -> Board
createBoard tilesWithAssignedPositions = Board
  $ map fst
  $ sortBy (comparing snd)
  $ map (second positionToIndex) (tilesWithAssignedPositions ++ fixedTiles)

positionsThatCanHaveTreasures :: Board -> [Position]
positionsThatCanHaveTreasures (Board tiles) = filter (not . isStartingPosition)
  $ map indexToPosition [0..(length tiles - 1)]


assignRandomPositionsToTreasures :: Board -> [Treasure] -> StdGen -> [(Treasure, Position)]
assignRandomPositionsToTreasures (Board tiles) treasures generator = let
  -- positions that can receive a treasure do not include the starting positions
  poolOfPositions = positionsThatCanHaveTreasures (Board tiles)
  -- create a list of equal length that sometimes contains a treasure, and sometimes doesn't
  poolOfTreasures = treasures ++ replicate (length poolOfPositions - length allTreasures) Nothing
  (shuffledTreasures, newGenerator) = shuffle poolOfTreasures generator
  -- [(Treasure, Position)]
  treasuresWithAssignedPositions = zip shuffledTreasures poolOfPositions
    ++ zip (replicate (length allStartingPositions) Nothing) allStartingPositions
  in treasuresWithAssignedPositions

putTreasuresOnBoard :: Board -> [(Treasure, Position)] -> Board
putTreasuresOnBoard (Board tiles) treasuresWithAssignedPositions = let
  -- [Treasure]
  treasures = map fst
    $ sortBy (comparing snd)
    $ map (second positionToIndex) treasuresWithAssignedPositions
  -- Create a new board taking into account the assigned treasures
  newBoard = Board
    $ map (\(Tile kind oldTreasure direction, newTreasure) -> Tile kind newTreasure direction)
    $ zip tiles treasures
  in newBoard

distributeTreasures :: Board -> [Treasure] -> StdGen -> Board
distributeTreasures board treasures generator = putTreasuresOnBoard board
  $ assignRandomPositionsToTreasures board treasures generator

{-|
 Creates a completely new board by
  1. Adding a random direction to the pool of free tiles
  2. Assigning a random free position to those tiles and creating an empty board
  3. Assigning treasures randomly to the non-starter tiles
-}
createNewBoard :: StdGen -> (Board, FreeTile)
createNewBoard generator = let
  (tiles, generator1) = createTiles poolOfFreeTiles generator
  -- There is 1 extra tile, so after shuffling, take the first tile away as the extra tile
  (Tile k t d : shuffledTiles, generator2) = shuffle tiles generator1
  extraTile = FreeTile k t
  -- Shuffle the pool of free positions and assign it to the shuffled tiles
  (shuffledPositions, generator3) = shuffle poolOfFreePositions generator2
  board = createBoard (zip shuffledTiles shuffledPositions)
  boardWithTreasures = distributeTreasures board (map Just allTreasures) generator3
  in (boardWithTreasures, extraTile)
