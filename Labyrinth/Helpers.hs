module Labyrinth.Helpers (
  replaceAtIndex,
  deleteAtIndex
) where

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex index item list = listUntilIndex ++ (item:listAfterIndex)
  where (listUntilIndex, _:listAfterIndex) = splitAt index list

deleteAtIndex :: Int -> [a] -> [a]
deleteAtIndex index list = left ++ right
    where (left, _:right) = splitAt index list
