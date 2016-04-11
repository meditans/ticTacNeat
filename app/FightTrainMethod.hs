-- | Questo modulo si occupa di esportare un TrainMethod che associa ad ogni
-- genoma la somma dei risultati ottenuti giocando contro avversari casuali
-- nella stessa popolazione.

module FightTrainMethod (fightTrainMethod) where

import           Control.Arrow         ((&&&))
import           Control.Lens          (element, (+~), (^?!))
import           Control.Monad         (replicateM)
import           Control.Monad.Random  (MonadRandom)
import           Data.Function         (on)
import           Data.List             (foldl1', groupBy, sortBy)
import           Data.Ord              (comparing)
import           Neet                  (Genome, TrainMethod (..))
import           System.Random.Shuffle (shuffleM)


-- | Dati due genomi, ritorna una coppia di valutazioni della performance
type ScoringFunction = Genome -> Genome -> (Double, Double)

-- | Data una funzione di scoring e un intero che descrive quanti round di
-- combattimento ci sono in ogni turno, si ha un training method.
fightTrainMethod :: (MonadRandom m) => ScoringFunction -> Int -> TrainMethod m
fightTrainMethod f nRounds = TrainMethod (\tGen -> do
  is <- concat <$> replicateM nRounds (pure     [0 .. length tGen - 1])
  js <- concat <$> replicateM nRounds (shuffleM [0 .. length tGen - 1])
  let fight i j = let (p1, p2) = f (tGen ^?! element i) (tGen ^?! element j)
                  in  [(i, p1), (j, p2)]
      scores = collect . concat $ zipWith fight is js
  return . updateWith scores $ fmap (const 0) tGen)

----- Funzioni di implementazione

collect :: (Ord a, Num b) => [(a,b)] -> [(a,b)]
collect = map (fst . head &&& sum . map snd)
        . groupBy ((==) `on` fst)
        . sortBy  (comparing fst)


updateWith :: (Num a, Traversable t) => [(Int, a)] -> t a -> t a
updateWith = foldl1' (.)
           . map (\(i, payoff) -> element i +~ payoff)
