{-# LANGUAGE ScopedTypeVariables #-}
module Chapter17.Final
  ( TicTacToe
  , winner
  , stupidAI
  ) where

import Prelude hiding (take)

import Control.Monad
import Control.Applicative
import Data.Maybe

import Chapter17.Types

class (Monad m) => TicTacToe m where
  info :: Position -> m (Maybe Player)
  take :: Position -> m Result

class (Monad m) => Term m where
  tget :: m String
  tput :: String -> m ()

game :: forall m. (Term m, TicTacToe m) => m ()
game = tput "an exciting game of tictactoe" >> round
  where
    fullround = do
      playerRound
      
  case player of
    Just player' -> tput $ "player " <> show player' <> " has won"
    Nothing -> round >> game
  where
    square =
      \case
        Just X -> 'x'
        Just O -> 'o'
        Nothing -> ' '
    draw = undefined
    round = undefined
      
winner :: forall m. TicTacToe m => m (Maybe Player)
winner = msum <$> mapM check winningcombos
  where
    same :: (Eq a) => [Maybe a] -> Maybe a
    same [] = Nothing
    same (x:xs) =
      if isJust x && all (==x) xs
      then x
      else Nothing
    winningcombos :: [[Position]]
    winningcombos = [ col n ++ row n | n <- [0..2]] ++ diags
    check :: [Position] -> m (Maybe Player)
    check pos = same <$> mapM info pos
    col n = [(n,y) | y <- [0..2]]
    row n = [(x,n) | x <- [0..2]]    
    diags = [ [(x,x)     | x <- [0..2]]
            , [(2-x,2-x) | x <- [0..2]]
            ]
            
stupidAI :: TicTacToe m => m Bool
stupidAI = go [(x,y) | x <- [0..2], y <- [0..2]]
  where go [] = pure False
        go (p:ps) = do
          taken <- isJust <$> info p
          if taken
            then go ps
            else take p >> pure True
