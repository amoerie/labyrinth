module Labyrinth.Models (
  labyrinthSize,
  Kind(..), Direction(..), Treasure, EmptyTile(..),
  FreeTile(..), Tile(..), Color(..), Control,
  Position, Cards, Player(..), Board(..), Game(..),
  hasOpening, inverse
) where

import Labyrinth.Helpers
import Prelude hiding (Right, Left)
import Data.Char
import Data.List.Split
import System.Random

labyrinthSize :: Int
labyrinthSize = 7

data Kind = Corner | TShape | Line deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show, Eq, Bounded, Enum)
type Treasure = Maybe Int
data EmptyTile = EmptyTile Kind Direction deriving(Eq)
data FreeTile = FreeTile Kind Treasure deriving (Show, Eq)
data Tile = Tile Kind Treasure Direction deriving (Eq)
data Color = Yellow | Red | Blue | Green deriving (Show, Eq, Bounded, Enum)
data Control = Human | AI deriving (Eq)
type Position = (Int, Int)
type Cards = [Int]
data Player = Player Color Control Position Cards deriving (Eq)
data Game = Game [Player] FreeTile Board
newtype Board = Board [Tile]

instance Show EmptyTile
  where show (EmptyTile Corner North) =  " | |_\n\
                                         \ |___\n\
                                         \     "

        show (EmptyTile Corner East) =   "  ___\n\
                                         \ |  _\n\
                                         \ | | "

        show (EmptyTile Corner South) =  "___  \n\
                                         \_  | \n\
                                         \ | | "

        show (EmptyTile Corner West) =   "_| | \n\
                                         \___| \n\
                                         \     "

        show (EmptyTile TShape North) =  "_| |_\n\
                                         \_____\n\
                                         \     "

        show (EmptyTile TShape East) =   " | |_\n\
                                         \ |  _\n\
                                         \ | | "

        show (EmptyTile TShape South) =  "_____\n\
                                         \_   _\n\
                                         \ | | "

        show (EmptyTile TShape West) =   "_| | \n\
                                         \_  | \n\
                                         \ | | "

        show (EmptyTile Line North) =    " | | \n\
                                         \ | | \n\
                                         \ | | "

        show (EmptyTile Line East) =     "_____\n\
                                         \_____\n\
                                         \     "

        show (EmptyTile Line South) =    " | | \n\
                                         \ | | \n\
                                         \ | | "

        show (EmptyTile Line West) =     "_____\n\
                                         \_____\n\
                                         \     "

treasureToChar :: Treasure -> Char
treasureToChar Nothing    = ' '
treasureToChar (Just idx) = ['a'..'z'] !! idx

instance Show Tile where
  show (Tile kind treasure direction) = replaceAtIndex 8 treasureAsChar asEmptyTile
    where asEmptyTile = show (EmptyTile kind direction)
          treasureAsChar = treasureToChar treasure

instance Random Direction where
    random gen = case randomR (fromEnum (minBound :: Direction), fromEnum (maxBound :: Direction)) gen
                 of (randomIndex, newGen) -> (toEnum randomIndex, newGen)
    randomR (lower,upper) g = case randomR (fromEnum lower, fromEnum upper) g
                      of (randomIndex, newGen) -> (toEnum randomIndex, newGen)

showRow :: [Tile] -> String
showRow rowOfTiles = unlines
  $ foldl (zipWith (++)) ["", "", ""]
  $ map (lines . show) rowOfTiles

instance Show Board where
  show (Board [])     = ""
  show (Board tiles)  = unlines
    $ map showRow
    $ chunksOf labyrinthSize tiles

type Rotation = Direction -> Direction
rotateClockWise :: Rotation
rotateClockWise North = East
rotateClockWise East  = South
rotateClockWise South = West
rotateClockWise West  = North

applyRotations :: Direction -> [Rotation] -> Direction
applyRotations = foldl (\dir rotation -> rotation dir)

getRotations :: Direction -> Direction -> [Rotation]
getRotations from to
  | from == to  = []
  | otherwise   = rotateClockWise : getRotations (rotateClockWise from) to

inverse :: Direction -> Direction
inverse North = South
inverse East  = West
inverse South = North
inverse West  = East

-- Determines whether a tile has an opening on the side of the provided direction
hasOpening :: Direction -> Tile -> Bool
-- predefined openings for the corner tile facing north
hasOpening North (Tile Corner _ North)  = True
hasOpening East  (Tile Corner _ North)  = True
hasOpening South (Tile Corner _ North)  = False
hasOpening West  (Tile Corner _ North)  = False
-- predefined openings for the tshape tile facing north
hasOpening North (Tile TShape _ North)  = True
hasOpening East  (Tile TShape _ North)  = True
hasOpening South (Tile TShape _ North)  = False
hasOpening West  (Tile TShape _ North)  = True
-- predefined openings for the line tile facing north
hasOpening North (Tile Line _ North)    = True
hasOpening East  (Tile Line _ North)    = False
hasOpening South (Tile Line _ North)    = True
hasOpening West  (Tile Line _ North)    = False
-- We can compute if a tile has an opening for other sides than North
-- by rotating the tile to the north
-- For example, to determine if a Corner tile facing East has a South opening
-- We can turn the tile to the North, which takes 3 clockwise rotations
-- We then apply those same 3 rotations to 'South'
-- So asking whether a Corner tile facing East has a South opening
-- is equivalent to asking whether a Corner tile facing North has an East opening
hasOpening direction (Tile kind t tileDirection) = hasOpening rotatedDirection (Tile kind t North)
  where rotations = getRotations tileDirection North
        rotatedDirection = applyRotations direction rotations
