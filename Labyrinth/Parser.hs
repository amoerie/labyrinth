module Labyrinth.Parser (apply, labyrinth) where

import qualified Data.Char
import qualified Control.Monad
import Labyrinth.Models

newtype Parser a = P (String -> [(a,String)])

apply :: Parser a -> String -> [(a,String)]
apply (P f) s = f s

fromRead :: Read a => Parser a
fromRead = P reads

instance Functor Parser where
  fmap f p = P (\s -> map (\(x,s) -> (f x, s)) $ apply p s)

instance Applicative Parser where
  -- Insert the value, let the input intact
  pure x = P (\s -> [(x,s)])
  p1 <*> p2 = P (\s -> do (f, s1) <- apply p1 s
                          (x, s2) <- apply p2 s1
                          return (f x, s2))

instance Monad Parser where
  return = pure
  -- Combine two parsers
  p >>= f = P (\s1 -> concat (map (\(x,s2) -> apply (f x) s2) (apply p s1)))

------------------------
-- Parser combinators --
------------------------

-- Combine all the possible reading
plus :: Parser a -> Parser a -> Parser a
plus p1 p2 = P (\s -> (apply p1 s) ++ (apply p2 s))

-- Indem plus but on a list of parser
allof :: [Parser a] -> Parser a
allof ps = P (\s -> concat $ map (\p -> apply p s) ps)

-- Try the first parser, in case of failure use the second parser
orelse :: Parser a -> Parser a -> Parser a
orelse p1 p2 = P (\s -> let result1 = apply p1 s in if null result1 then apply p2 s else result1)

-- Idem orelse but on a list of parser
oneof :: [Parser a] -> Parser a
oneof [] = zero
oneof (p:ps) = p `orelse` (oneof ps)

-- Use the same parser again and again until failure
many :: Parser a -> Parser [a]
many p = orelse
           (do x <- p
               xs <- many p
               return $ x:xs)
           (return [])

-- Idem many, but require at least one parsing
some :: Parser a -> Parser [a]
some p = do x <- p
            xs <- many p
            return $ x:xs

--------------------
-- Simple Parsers --
--------------------

-- The failure parser
zero :: Parser a
zero = P (\s -> [])

-- Eat a character
item :: Parser Char
item = P f where f [] = []
                 f (c:cs) = [(c,cs)]

-- Eat a character only if it satisfies
sat :: (Char -> Bool) -> Parser Char
sat f = item >>= (\c -> if f c then return c else zero)

-- Try to eat the given character
char :: Char -> Parser ()
char c = sat (==c) >> return ()

-- Try to eat the given string
string :: String -> Parser ()
string (c:cs) = char c >> string cs
string [] = return ()

-- Parse a natural
natural :: Parser Int
natural = do blank
             digits <- some (sat Data.Char.isDigit)
             return $ read digits
-------------
-- Helpers --
-------------

-- Eat blanks characters
blank :: Parser ()
blank = many (sat (\c -> c `elem` [' ', '\t', '\n'])) >> return ()

-- Eat blank then call string parser
keyword :: String -> Parser ()
keyword s = blank >> string s

-- Eat blank then parse alphabetical characters
token :: Parser String
token = blank >> (some $ sat Data.Char.isAlpha)

-----------------------
-- Labyrinth Parsers --
-----------------------

labyrinth :: Parser Game
labyrinth = do
  ps <- players
  ft <- freeTile
  brd <- board
  return $ Game ps ft brd

players :: Parser [Player]
players = many player

player :: Parser Player
player = do
  clr <- color
  ctrl <- control
  pos <- position
  crds <- cards
  return $ Player clr ctrl pos crds

color :: Parser Color
color = oneof [
  keyword "yellow" >> return Yellow,
  keyword "red"    >> return Red,
  keyword "blue"   >> return Blue,
  keyword "green"  >> return Green
  ]

control :: Parser Control
control = oneof [
  keyword "human" >> return Human,
  keyword "ai"    >> return AI
  ]

position :: Parser Position
position = do
  x <- natural
  y <- natural
  return (x,y)

cards :: Parser Cards
cards = many natural

board :: Parser Board
board = do
  ts <- tiles
  return $ Board ts

tiles :: Parser [Tile]
tiles = many tile

tile :: Parser Tile
tile = do
  k <- kind
  t <- treasure
  d <- direction
  return $ Tile k t d

freeTile :: Parser FreeTile
freeTile = do
  k <- kind
  t <- treasure
  return $ FreeTile k t

treasure :: Parser Treasure
treasure = orelse (do
                     number <- natural
                     return $ Just number)
                  (return Nothing)

kind :: Parser Kind
kind = oneof [
  keyword "corner" >> return Corner,
  keyword "tshape" >> return TShape,
  keyword "line"   >> return Line]

direction :: Parser Direction
direction = oneof [
  keyword "north" >> return North,
  keyword "south" >> return South,
  keyword "west"  >> return West,
  keyword "east"  >> return East]
