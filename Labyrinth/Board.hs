module Labyrinth.Board (
  InsertionPoint(..),
  insert
)
where

import Prelude hiding (Right, Left)
import Data.List hiding (insert)
import Labyrinth.Helpers
import Labyrinth.Models
import Labyrinth.Factory

data InsertionPoint = InsertionPoint Position Direction
type Move = Position -> Position

up :: Move
up (x,y) = (x, y - 1)

down :: Move
down (x,y) = (x, y + 1)

left :: Move
left (x,y)  = (x - 1, y)

right :: Move
right (x,y) = (x + 1, y)

isValid :: Board -> Position -> Bool
isValid (Board []) pos    = False
isValid (Board tiles) pos = 0 <= index && index < length tiles
  where index = positionToIndex pos

insertionPoints :: [InsertionPoint]
insertionPoints = [
    InsertionPoint (0,1) South,
    InsertionPoint (0,3) South,
    InsertionPoint (0,5) South,

    InsertionPoint (1,0) East,
    InsertionPoint (3,0) East,
    InsertionPoint (5,0) East,

    InsertionPoint (1,6) West,
    InsertionPoint (3,6) West,
    InsertionPoint (5,6) West,

    InsertionPoint (6,1) North,
    InsertionPoint (6,3) North,
    InsertionPoint (6,5) North
  ]

directionToMove :: Direction -> Move
directionToMove North = up
directionToMove South = down
directionToMove East  = right
directionToMove West  = left

-- Returns an infinite list of positions that arise from applying the same move over and over
repeatMove :: Position -> Move -> [Position]
repeatMove pos move = newPos : repeatMove newPos move
  where newPos = move pos

-- Returns - in order - the positions of the tiles that will be modified
-- when a free tile is inserted at the provided insertion point
getAffectedPositions :: Board -> InsertionPoint -> [Position]
getAffectedPositions board (InsertionPoint pos direction) = takeWhile isValidOnBoard positions
  where move = directionToMove direction
        positions = repeatMove pos move
        isValidOnBoard = isValid board

{-
  Swaps the tile on the board in the provided position
  with the provided tile, and returns the new board + the tile that was removed from the board
-}
swap :: Board -> Position -> Tile -> (Board, Tile)
swap (Board tiles) pos tile = let
  index = positionToIndex pos
  otherTile = tiles !! index
  in (Board $ replaceAtIndex index tile tiles, otherTile)

{-
  Cousin of the infamous mapReduce, swapReduce will recursively keep swapping tiles
  until there are no more positions left to swap with
-}
swapReduce :: Board -> [Position] -> Tile -> (Board, Tile)
swapReduce board []     tile = (board, tile)
swapReduce board (p:ps) tile = let (newBoard, swappedTile) = swap board p tile
                               in swapReduce newBoard ps swappedTile

{-
 Inserts a tile at the provided insertion point and returns the new board + the new free tile
-}
insert :: Board -> InsertionPoint -> Tile -> (Board, FreeTile)
insert board insertionPoint tile = (newBoard, tileToFreeTile extraTile)
  where affectedPositions = getAffectedPositions board insertionPoint
        (newBoard, extraTile) = swapReduce board affectedPositions tile
