{-# OPTIONS_GHC -fdefer-typed-holes #-}

module TicTacToe where

import           Data.List   (maximumBy, intersperse)
import           Data.Map    (Map, empty, insert, lookup)
import           Data.Maybe  (isJust)
import           Data.Monoid (First (..), getFirst, (<>))
import           Data.Ord    (comparing)
import           Neet
import           Prelude     hiding (lookup)

type ScoringFunction = Genome -> Genome -> (Double, Double) --TODO move to main module?

data Player = X | O deriving (Eq, Show)

opponent :: Player -> Player
opponent X = O
opponent O = X

type BoardPos = Int
type Board    = Map BoardPos Player

move :: Player -> BoardPos -> Board -> Maybe Board
move p xy b | isJust (lookup xy b) = Nothing
            | otherwise            = Just (insert xy p b)

getWinner :: Board -> Maybe Player
getWinner b = getFirst (r0 <> r1 <> r2 <> c0 <> c1 <> c2 <> d0 <> d1)
  where
    r0 = check 0 1 2
    r1 = check 3 4 5
    r2 = check 6 7 8
    c0 = check 0 3 6
    c1 = check 1 4 7
    c2 = check 2 5 8
    d0 = check 0 4 8
    d1 = check 2 4 6
    check xy1 xy2 xy3 | lookup xy1 b == lookup xy2 b
                     && lookup xy1 b == lookup xy3 b = First (lookup xy1 b)
                      | otherwise                    = First Nothing

play :: (Board -> BoardPos) -> (Board -> BoardPos) -> (Double, Double)
play = play' X empty
  where
    play' turn board p1 p2 = case winner of Nothing -> scores
                                            Just X -> (6,4)
                                            Just O -> (4,6)
      where
        winner = getWinner board
        moveResult = case turn of X -> move X (p1 board) board
                                  O -> move O (p2 board) board
        --TODO refactor this mess
        scores = if all isJust $ map (flip lookup board) [0..8] then (5,5) --draw
                   else scores'
        scores' = case moveResult of Nothing -> failed turn
                                     Just b -> play' (opponent turn) b p1 p2
        failed X = (0,5)
        failed O = (5,0)

scoreTicTacToe :: ScoringFunction
scoreTicTacToe g1 g2 = play (abstractGenome g1 X) (abstractGenome g2 O)

abstractGenome :: Genome -> (Player -> Board -> BoardPos)
abstractGenome g p board = maximumBy (comparing (outputs !!)) [0..8]
  where
    net     = mkPhenotype g
    inputs  = flip map [0..8] (\i -> case lookup i board of
                Nothing -> 0
                Just p' -> if p' == p then 1 else -1)
    outputs = getOutput (snapshot net inputs)

showBoard :: Board -> String
showBoard b = (concat . intersperse "\n")
            $ fmap (concat . intersperse "|")
            $ (fmap (fmap (showCell . (flip lookup b))))
            $ cells
  where
    cells = [[0,1,2],[3,4,5],[6,7,8]]
    showCell Nothing = " "
    showCell (Just X) = "X"
    showCell (Just O) = "O"

interactivePlay :: (Player -> Board -> BoardPos) -> IO ()
interactivePlay f = do
  let loop b = do
                 let nnMove = f X b
                 let b' = insert nnMove X b
                 putStrLn ("NN moved: " ++ show nnMove)
                 putStrLn $ showBoard b'
                 print "Your move >"
                 playerMove <- read <$> getLine
                 let b'' = insert playerMove O b'
                 loop b''
  loop empty
