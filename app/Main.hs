-- * Descrizione dell'applicazione
-- Evolviamo un giocatore di tic/tac/toe usando neat

{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

-- import TicTacToe
import FightTrainMethod

import Control.Monad.Random
import Neet

main :: IO ()
main = do
  trainedPop <- evalRandIO $ trainN (fightTrainMethod _what 3) 100 ticTacPopulation
  putStrLn "Here's the best player after this training:"
  renderGenome (popBOrg trainedPop)

ticTacPopulation :: Population
ticTacPopulation = newPop 100 PS { psSize = 100
                                 , psInputs = 9
                                 , psOutputs = 9
                                 , psParams = defParams
                                 , sparse = Nothing
                                 , psStrategy = Nothing
                                 }

