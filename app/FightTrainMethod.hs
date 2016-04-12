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
  let scores = collect . concat $ zipWith (partialScore tGen f) is js
      emptyScores = fmap (const 0) tGen
  return . updateWith scores $ emptyScores)

----- Funzioni di implementazione

-- Internamente, usiamo il concetto di PartialScore, che e' una coppia formata
-- da un indice e una valutazione.
type PartialScore = (Int, Double)

-- La funzione partialScore crea i due punteggi parziali relativi ad uno scontro,
-- data la struttura, la funzione di valutazione e i due indici.
partialScore :: Traversable t => t Genome -> ScoringFunction -> Int -> Int -> [PartialScore]
partialScore t f i j = [(i, payoff1), (j, payoff2)]
  where (payoff1, payoff2) = f (t ^?! element i) (t ^?! element j)

-- La funzione collect gestisce una lista di PartialScore eliminando la
-- duplicazione, ovvero rendendo unico l'indice di riferimento e sommando i
-- punteggi parziali.
collect :: [PartialScore] -> [PartialScore]
collect = map (fst . head &&& sum . map snd)
        . groupBy ((==) `on` fst)
        . sortBy  (comparing fst)

-- La funzione updateWith trasforma una lista di partialscores in una funzione
-- che modifica i rispettivi elementi della struttura traversabile.
updateWith :: (Traversable t) => [PartialScore] -> t Double -> t Double
updateWith = foldl1' (.) . map (\(i, payoff) -> element i +~ payoff)
