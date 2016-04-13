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

type Strategy = Player -> Board -> BoardPos

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
play = play' X empty 0
  where
    play' turn board step p1 p2 =
      case (winner,  draw,  moveResult)
        of (Just X,  _,     _         ) -> (6,4) -- X won
           (Just O,  _,     _         ) -> (4,6) -- O won
           (_,       True,  _         ) -> (5,5) -- draw
           (_,       _,     Nothing   ) -> if turn == X then (step/2,5) else (5,step/2) -- illegal move
           (_,       _,     Just b    ) -> play' (opponent turn) b (step+1) p1 p2 -- continue the game
      where
        winner = getWinner board
        draw = all isJust $ map (`lookup` board) [0..8]
        movePos = (if turn == X then p1 else p2) board
        moveResult = move turn movePos board

scoreTicTacToe :: ScoringFunction
scoreTicTacToe g1 g2 = play (abstractGenome g1 X) (abstractGenome g2 O)

abstractGenome :: Genome -> Strategy
abstractGenome g p board = maximumBy (comparing (outputs !!)) [0..8]
  where
    net     = mkPhenotype g
    inputs  = flip map [0..8] (\i -> case lookup i board of
                Nothing -> 0
                Just p' -> if p' == p then 1 else -1)
    outputs = pushThrough net inputs

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

interactivePlay :: Strategy -> IO ()
interactivePlay f = do
  let loop b = do
                 let nnMove = f X b
                 let b' = insert nnMove X b
                 putStrLn ("NN moved: " ++ show nnMove)
                 putStrLn $ showBoard b'
                 putStr "Your move >"
                 playerMove <- read <$> getLine
                 let b'' = insert playerMove O b'
                 loop b''
  loop empty
