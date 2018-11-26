module Chapter17.Types
  ( Player(..)
  , Result(..)
  , Position
  , Board
  ) where

import Data.Map.Strict

data Player = X | O deriving (Show, Eq)
data Result = AlreadTaken | Taken deriving Show
type Position = (Int,Int) -- x,y
type Board = Map Position Player
