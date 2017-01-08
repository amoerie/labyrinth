module Labyrinth.Models (
  Kind(..), Direction(..), Treasure, EmptyTile(..),
  XTile(..), Tile(..), Color, Control,
  Position, Cards, Player, UnfinishedBoard, Board
) where

import Labyrinth.Helpers
import Prelude hiding (Right, Left)
import Data.Char

data Kind = Corner | TShape | Line deriving (Eq)
data Direction = North | East | South | West deriving (Eq)
type Treasure = Maybe Int
data EmptyTile = EmptyTile Kind Direction deriving(Eq)
data XTile = XTile Kind Treasure deriving (Eq)
data Tile = Tile Kind Treasure Direction deriving (Eq)
data Color = Yellow | Red | Blue | Green deriving (Eq)
data Control = Human | AI deriving (Eq)
type Position = (Int, Int)
type Cards = [Int]
data Player = Player Color Control Position Cards deriving (Eq)
type UnfinishedBoard = [Maybe Tile]
type Board = [Tile]

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



instance Show Tile where
  show (Tile kind Nothing direction) = show (EmptyTile kind direction)
  show (Tile kind (Just treasure) direction) = replaceAtIndex 8 treasureAsChar asEmptyTile
    where asEmptyTile = show (EmptyTile kind direction)
          treasureAsChar = intToDigit treasure
