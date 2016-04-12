module TicTacToe where

import Data.Map
import Prelude hiding (lookup)
import Data.Monoid
import Data.Maybe (isJust)

data Genome = Genome --test

type ScoringFunction = Genome -> Genome -> (Double, Double) --TODO move to main module?

data Player = X | O deriving (Enum, Eq, Show)

type Board = Map (Int, Int) Player

move :: Player -> (Int,Int) -> Board -> Maybe Board
move p xy b | isJust $ lookup xy b = Nothing --se c'è già qualcosa la mossa non è valida
            | otherwise = Just $ insert xy p b

getWinner :: Board -> Maybe Player
getWinner b = getFirst (r0 <> r1 <> r2 <> c0 <> c1 <> c2 <> d0 <> d1)
  where
    r0 = check (0,0) (1,0) (2,0)
    r1 = check (0,1) (1,1) (2,1)
    r2 = check (0,2) (1,2) (2,2)
    c0 = check (0,0) (0,1) (0,2)
    c1 = check (1,0) (1,1) (1,2)
    c2 = check (2,0) (2,1) (2,2)
    d0 = check (0,0) (1,1) (2,2)
    d1 = check (2,0) (1,1) (0,2)
    check xy1 xy2 xy3 | lookup xy1 b == lookup xy2 b && lookup xy1 b == lookup xy3 b = First $ lookup xy1 b
                      | otherwise = First Nothing

play :: (Board -> (Int,Int)) -> (Board -> (Int,Int)) -> (Double, Double)
play = play' X empty
  where
    play' turn board p1 p2 = case winner of Nothing -> scores
                                            Just X -> (1,-1)
                                            Just O -> (-1,1)
      where
        winner = getWinner board
        moveResult = case turn of X -> move X (p1 board) board
                                  O -> move O (p2 board) board
        scores = case moveResult of Nothing -> failed turn
                                    Just b -> play' (succ turn) b p1 p2
        failed X = (-2,0)
        failed O = (0,-2)

genomeToFunctionThatTakesABoardAndReturnsAMove :: Player -> Genome -> (Board -> (Int,Int))
genomeToFunctionThatTakesABoardAndReturnsAMove = undefined --TODO esecuzione della rete neurale

scoreTicTacToe :: ScoringFunction
scoreTicTacToe g1 g2 = play
                         (genomeToFunctionThatTakesABoardAndReturnsAMove X g1)
                         (genomeToFunctionThatTakesABoardAndReturnsAMove O g2)

