-- * Descrizione dell'applicazione
-- Evolviamo un giocatore di tic/tac/toe usando neat

{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import Neet

type ScoringFunction = Genome -> Genome -> (Double, Double)

fightTrainMethod :: (MonadRandom m) => ScoringFunction -> TrainMethod m
fightTrainMethod f tGen = _what

main :: IO ()
main = putStrLn "hi"

