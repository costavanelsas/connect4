module Game
  ( gameLoop
  , hasWinner
  , GameState(..)
  ) where

import Types
import Board
import System.Random (randomRIO)

data GameState = Win Player | Draw | Ongoing

gameState :: Board -> GameState
gameState b
    | hasWinner b Red    = Win Red
    | hasWinner b Yellow = Win Yellow
    | isFull b           = Draw
    | otherwise          = Ongoing


gameLoop :: Board -> Player -> IO ()
gameLoop board player = do
    putStrLn (renderBoard board)

    case gameState board of
      Win p -> putStrLn (show p ++ " wins!")
      Draw  -> putStrLn "Draw."
      Ongoing -> turn board player


turn :: Board -> Player -> IO ()
turn board player =
    if player == Red
      then humanTurn
      else aiTurn
  where

    humanTurn = do
      putStrLn ("Turn: " ++ show player ++ ". Choose column:")
      col <- readMove
      handleMove col

    aiTurn = do
      putStrLn "AI is thinking..."
      col <- chooseAIMove board
      handleMove col

    handleMove col =
      case dropDisc player col board of
        Nothing -> do
            putStrLn "Invalid column. Try again."
            turn board player
        Just newBoard ->
            gameLoop newBoard (switchPlayer player)


readMove :: IO Column
readMove = do
    line <- getLine
    case reads line of
      [(n, "")] -> return n
      _         -> do
          putStrLn "Enter a valid number."
          readMove


chooseAIMove :: Board -> IO Column
chooseAIMove board =
    case winningMove Yellow of
      Just c  -> return c
      Nothing -> randomValid
  where
    validCols =
      [ c | c <- [0..cols-1]
          , dropDisc Yellow c board /= Nothing ]

    randomValid =
      if null validCols
        then return 0
        else do
            i <- randomRIO (0, length validCols - 1)
            return (validCols !! i)

    winningMove p =
      let w = [ c | c <- [0..cols-1]
                  , Just b' <- [dropDisc p c board]
                  , hasWinner b' p ]
      in case w of
           (c:_) -> Just c
           _     -> Nothing


hasWinner :: Board -> Player -> Bool
hasWinner b p =
    any (has4 p) (allLines b)

has4 :: Player -> [Cell] -> Bool
has4 p cells =
    any (all (== Taken p)) (windows 4 cells)

windows :: Int -> [a] -> [[a]]
windows n xs =
  case xs of
    [] -> []
    (_:rest)
      | length xs < n -> []
      | otherwise     -> take n xs : windows n rest



allLines :: Board -> [[Cell]]
allLines b =
    horizontals ++ verticals ++ diagonals1 ++ diagonals2
  where
    horizontals = b
    verticals   = rowsToCols b
    diagonals1  = diagonals b
    diagonals2  = diagonals (map reverse b)

rowsToCols :: [[a]] -> [[a]]
rowsToCols [] = []
rowsToCols xs
  | any null xs = []
  | otherwise =
      let heads = [ h | (h:_) <- xs ]
          tails = [ t | (_:t) <- xs ]
      in heads : rowsToCols tails



diagonals :: [[a]] -> [[a]]
diagonals b =
    [ [ b !! (r+i) !! (c+i)
      | i <- [0..min (rows-r-1) (cols-c-1)] ]
    | r <- [0..rows-1], c <- [0..cols-1] ]
