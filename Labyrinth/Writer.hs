-- Not entirely sure if the writer monad is necessary here, I opted against it

module Labyrinth.Writer(
 writeLabyrinth
)

where

import qualified Data.List
import Labyrinth.Models

joinWithSpaces :: [String] -> String
joinWithSpaces = Data.List.unwords

joinWithLines :: [String] -> String
joinWithLines = Data.List.unlines

writeLabyrinth :: Game -> String
writeLabyrinth (Game players freeTile board) = joinWithLines
  $ writePlayers players : writeFreeTile freeTile : [writeBoard board]

writePlayers :: [Player] -> String
writePlayers = joinWithLines . map writePlayer

writePlayer :: Player -> String
writePlayer (Player color control position cards) = joinWithSpaces
  $ writeColor color : writeControl control : writePosition position : [writeCards cards]

writeColor :: Color -> String
writeColor Yellow = "yellow"
writeColor Red    = "red"
writeColor Blue   = "blue"
writeColor Green  = "green"

writeControl :: Control -> String
writeControl Human = "human"
writeControl AI    = "ai"

writePosition :: Position -> String
writePosition (x,y) = joinWithSpaces $ show x : [show y]

writeCards :: Cards -> String
writeCards cards = joinWithSpaces $ map show cards

writeFreeTile :: FreeTile -> String
writeFreeTile (FreeTile kind treasure) = joinWithSpaces
  $ writeKind kind : [writeTreasure treasure]

writeKind :: Kind -> String
writeKind Corner = "corner"
writeKind TShape = "tshape"
writeKind Line   = "line"

writeTreasure :: Treasure -> String
writeTreasure (Just t) = show t
writeTreasure Nothing  = ""

writeBoard :: Board -> String
writeBoard (Board tiles) = writeTiles tiles

writeTiles :: [Tile] -> String
writeTiles = joinWithLines . map writeTile

writeTile :: Tile -> String
writeTile (Tile kind treasure direction) = joinWithSpaces
  $ writeKind kind : writeTreasure treasure : [writeDirection direction]

writeDirection :: Direction -> String
writeDirection North = "north"
writeDirection East = "east"
writeDirection South = "south"
writeDirection West = "west"
