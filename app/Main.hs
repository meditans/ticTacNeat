-- * Descrizione dell'applicazione
-- Evolviamo un giocatore di tic/tac/toe usando neat

{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import           Control.Monad.Random (evalRandIO)
import qualified Data.ByteString      as BS
import           Data.Serialize       (encode)
import           FightTrainMethod     (fightTrainMethod)
import           Neet
import           TicTacToe            (scoreTicTacToe)

main :: IO ()
main = do
  trainedPop <- iterateM 1000 ticTacPopulation (\p -> do
    putStrLn $! "Evaluating generation " ++ show (popGen p)
    putStrLn $! "The best score in this generation is " ++ show (popBScore p)
    p' <- evalRandIO $ trainOnce (fightTrainMethod scoreTicTacToe 3) p
    return p')
  putStrLn "Encoding the best genome..."
  BS.writeFile "genome" (encode $ popBOrg trainedPop)
  putStrLn "Done"

ticTacPopulation :: Population
ticTacPopulation = newPop 100 PS { psSize = 100
                                 , psInputs = 9
                                 , psOutputs = 9
                                 , psParams = defParams
                                 , sparse = Just 5
                                 , psStrategy = Nothing
                                 }

iterateM :: Monad m => Int -> a -> (a -> m a) -> m a
iterateM 0 a _ = return a
iterateM n a f = f a >>= (\a' -> iterateM (n-1) a' f)
