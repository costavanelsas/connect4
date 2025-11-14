module Types
  ( module Types
  ) where

data Player = Red | Yellow
  deriving (Eq, Show)

type Column = Int

data Cell = Empty | Taken Player
  deriving (Eq, Show)

type Board = [[Cell]]
