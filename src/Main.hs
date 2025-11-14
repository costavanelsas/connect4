module Main (main) where

import Board
import Game
import Types

main :: IO ()
main = do
    putStrLn "Connect 4"
    gameLoop emptyBoard Red
