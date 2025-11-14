module Board
  ( rows
  , cols
  , emptyBoard
  , dropDisc
  , renderBoard
  , switchPlayer
  , isFull
  ) where

import Types

rows, cols :: Int
rows = 6
cols = 7

emptyBoard :: Board
emptyBoard = replicate rows (replicate cols Empty)

switchPlayer :: Player -> Player
switchPlayer Red    = Yellow
switchPlayer Yellow = Red

isFull :: Board -> Bool
isFull b = all (all (/= Empty)) b

setCell :: Int -> Int -> Cell -> Board -> Board
setCell r c val board =
  let row = board !! r
      newRow = replace c val row
  in take r board ++ [newRow] ++ drop (r + 1) board

replace :: Int -> a -> [a] -> [a]
replace i v xs = take i xs ++ [v] ++ drop (i + 1) xs

dropDisc :: Player -> Column -> Board -> Maybe Board
dropDisc player col board
  | col < 0 || col >= cols = Nothing
  | otherwise =
      case findLowestEmptyRow (rows - 1) of
        Nothing -> Nothing
        Just r  -> Just (setCell r col (Taken player) board)
  where
    findLowestEmptyRow (-1) = Nothing
    findLowestEmptyRow r =
      if (board !! r) !! col == Empty
         then Just r
         else findLowestEmptyRow (r - 1)

renderBoard :: Board -> String
renderBoard b =
  unlines (map showRow b) ++ colNumbers
  where
    showCell Empty        = "."
    showCell (Taken Red)    = "R"
    showCell (Taken Yellow) = "Y"

    showRow row = unwords (map showCell row)
    colNumbers = unwords (map show [0 .. cols - 1]) ++ "\n"
