module Labyrinth.Helpers (
  replaceAtIndex,
  deleteAtIndex,
  shuffle
) where

import System.Random

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex index item list = listUntilIndex ++ (item:listAfterIndex)
  where (listUntilIndex, _:listAfterIndex) = splitAt index list

deleteAtIndex :: Int -> [a] -> [a]
deleteAtIndex index list = left ++ right
    where (left, _:right) = splitAt index list

shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle []   generator = ([], generator)
shuffle list generator = let (index,nextGenerator) = randomR (0,length list -1) generator
                             (listUntilIndex, element:listAfterIndex) = splitAt index list
                             (shuffledRest, lastGenerator) = shuffle (listUntilIndex ++ listAfterIndex) nextGenerator
                         in  (element : shuffledRest, lastGenerator)
