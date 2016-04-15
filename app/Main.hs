-- * Descrizione dell'applicazione
-- Evolviamo un giocatore di tic/tac/toe usando neat

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import           Control.Monad.Random (evalRandIO)
import qualified Data.ByteString      as BS
import           Data.Serialize       (decode, encode)
import           FightTrainMethod     (fightTrainMethod)
import           Neet
import           Options.Generic
import           TicTacToe            (abstractGenome, interactivePlay,
                                       scoreTicTacToe)

data Options = Options { nGens :: Maybe Int <?> "Number of generations" }
               deriving (Generic, Show)

instance ParseRecord Options

main :: IO ()
main = do
  opts <- getRecord "Train a tic-tac-toe player using the NEAT architecture"
  let gens = maybe 1000 id (unHelpful $ nGens opts)
  trainedPop <- iterateM gens ticTacPopulation (\p -> do
    putStrLn $! "Evaluating generation " ++ show (popGen p)
    putStrLn $! "The best score in this generation is " ++ show (popBScore p)
    evalRandIO $! trainOnce (fightTrainMethod scoreTicTacToe 3) p)
  putStrLn "Encoding the best genome..."
  BS.writeFile "genome" (encode $ popBOrg trainedPop)
  putStrLn "Done"

ticTacPopulation :: Population
ticTacPopulation = newPop 100 PS { psSize = 100
                                 , psInputs = 27
                                 , psOutputs = 9
                                 , psParams = defParams
                                 , sparse = Just 5
                                 , psStrategy = Nothing
                                 }

iterateM :: Monad m => Int -> a -> (a -> m a) -> m a
iterateM 0 a _ = return a
iterateM n a f = f a >>= (\a' -> iterateM (n-1) a' f)

testGenome :: String -> IO ()
testGenome file = do
  bs <- BS.readFile file
  let (Right g) = decode bs
  let f = abstractGenome g
  interactivePlay f
