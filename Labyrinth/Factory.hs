module Labyrinth.Game (
  labyrinthSize,
  startingLabyrinth
)
where

{-|
  The factory is responsible for starting a completely new game.
  It draws a new board and distributes the free tiles randomly
-}

import Labyrinth.Models
import Labyrinth.Helpers

import qualified System.Random
import Control.Monad.State

labyrinthSize :: Int
labyrinthSize = 7

{-|
  A completely empty labyrinth, containing no tiles
-}
emptyLabyrinth :: UnfinishedBoard
emptyLabyrinth = replicate (labyrinthSize * labyrinthSize) Nothing

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
    (Tile TShape Nothing South,(6,2)),
    (Tile TShape Nothing South,(6,4)),
    (Tile Corner Nothing West, (6,6))
  ]

positionToIndex :: Position -> Int
positionToIndex (x,y) = y * labyrinthSize + x

indexToPosition :: Int -> Position
indexToPosition index = (index `mod` labyrinthSize, index `div` labyrinthSize)

putTile :: UnfinishedBoard -> (Tile, Position) -> UnfinishedBoard
putTile []     _           = []
putTile board  (tile, pos) = replaceAtIndex index (Just tile) board
  where index = positionToIndex pos

{-|
  The starting labyrinth, containing only the fixed tiles
-}
startingLabyrinth :: UnfinishedBoard
startingLabyrinth = foldl putTile emptyLabyrinth fixedTiles

type XTiles = [XTile]
freeTiles :: XTiles
freeTiles = replicate 16 (XTile Corner Nothing)
  ++ replicate 6 (XTile TShape Nothing)
  ++ replicate 12 (XTile Line Nothing)

type FreePositions = [Position]
freePositions :: FreePositions
freePositions = map indexToPosition
  $ filter (`notElem` fixedIndex) [0..(labyrinthSize * labyrinthSize - 1)]
  where fixedIndex = map (positionToIndex . snd) fixedTiles

{-|
   Taking a free tile by index returns that free tile PLUS the remaining free tiles
   To achieve this, this function makes use of the State Monad
-}
takeFreeTile :: Int -> State XTiles XTile
takeFreeTile index = state $ \tiles -> (tiles!!index, deleteAtIndex index tiles)

{-|
   Taking a free position by index returns that position PLUS the remaining free positions
-}
takeFreePosition :: Int -> State [Position] Position
takeFreePosition index = state $ \ps -> (ps!!index, deleteAtIndex index ps)
