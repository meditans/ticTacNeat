{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- * Descrizione dell'applicazione
-- Evolviamo un giocatore di tic/tac/toe usando neat

module Main where

import           Control.Monad.Random (evalRandIO)
import qualified Data.ByteString      as BS
import           Data.Serialize       (encode, decode)
import           FightTrainMethod     (fightTrainMethod)
import           Neet
import           TicTacToe            (scoreTicTacToe, abstractGenome, interactivePlay)
import           System.Console.GetOpt
import           System.Exit
import           System.Environment
import           Text.Read            (readMaybe)


-- | Runtime and command line options. 
data RunTimeOptions
    = RunTimeOptions {
        rtOpts_iterations :: Int
      , rtOpts_commandLineError :: Maybe String
      } deriving (Show)

rtOpts_default :: RunTimeOptions
rtOpts_default
  = RunTimeOptions {
      rtOpts_iterations = 1000
    , rtOpts_commandLineError = Nothing
    }

declareRunTimeOptions :: [OptDescr (RunTimeOptions -> RunTimeOptions)]
declareRunTimeOptions =
  [ Option
      []
      ["iterations"]
      (ReqArg (\is opts
                   -> case readMaybe is of
                        Just i -> opts { rtOpts_iterations = i }
                        Nothing -> opts { rtOpts_commandLineError = Just "number of iterations is not a number"}
              )
       "NUM")
      "number of iterations"
  ]

parseRunTimeOptions :: [String] -> Either String (RunTimeOptions, [String])
parseRunTimeOptions argv =
  let parse1
        = case getOpt Permute declareRunTimeOptions argv of
            (o,n,[]) -> Right (foldl (flip id) rtOpts_default o, n)
            (_,_,errs) -> Left $ (concat errs ++ info)
      usageHeader = "Usage: "
      info = "\n" ++ (usageInfo usageHeader declareRunTimeOptions)
  in case parse1 of
       Left err -> Left err
       Right (opts, n)
           -> case rtOpts_commandLineError opts of
                Nothing -> Right (opts, n)
                Just err -> Left (err ++ info)


main :: IO ()
main = do
  cmdLineOpts <- getArgs
  (opts, nextArgs)
      <- case parseRunTimeOptions cmdLineOpts of
            Left err -> die err
            Right r -> return r

  trainedPop <- iterateM (rtOpts_iterations opts) ticTacPopulation (\p -> do
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

testGenome :: String -> IO ()
testGenome file = do
  bs <- BS.readFile file
  let (Right g) = decode bs
  let f = abstractGenome g
  interactivePlay f
