module Selection (
  --select,
  SelectionMethod,
  eliteSelection,
  randomSelection,
  rouletteSelection,
  rankingSelection,
  universalSelection,
  tournamentDeterministicSelection,
  tournamentStochasticSelection
)where

import Data.Ord
import Data.Sort
import System.Random

import Random
import GABase
import Utils

-- eliteSelection ; sort then take first k elems DONE
-- roulette ; upper ^ DONE
-- ranking ; inverse sort, as roulette where total_fitness = (n + 1) * n / 2 -- see even DONE
-- tournament deterministic; select random 2; compare
-- tournament stochastic; select random 2; invert compare with if
-- universal ; see with detail, should be base in roulette DONE
-- boltzmann ; if active, its a f to the fitness

type SelectionMethod = Population -> Int -> FitnessFunction -> Seed -> [Chromosome]

eliteSelection :: SelectionMethod
eliteSelection pop k fitness seed = take k (sortOn (Data.Ord.Down . fitness) pop ) --HLINT https://github.com/ndmitchell/hlint/blob/master/data/hlint.yaml

randomSelection :: SelectionMethod
randomSelection pop k fitness seed = take k (map snd (sortOn (Data.Ord.Down . fst) [(selectRands!!i, pop!!i) | i <- [0..(popSize-1)]])) where
  selectRands = fst (randInts seed popSize)
  popSize = length pop

rouletteSelection :: SelectionMethod
rouletteSelection pop k fitness seed = [pieFitnessSubselect fitness pop total (selectRands!!i) | i <- [0..(k-1)]] where
  selectRands = fst (randDoubles seed k)
  popSize = length pop
  total = fromIntegral (sum (map fitness pop))

universalSelection :: SelectionMethod
universalSelection pop k fitness seed = [pieFitnessSubselect fitness pop total (selectedFitness i k) | i <- [0..(k-1)]] where
  selectedRand = randDouble seed
  popSize = length pop
  total = fromIntegral (sum (map fitness pop))
  unfilteredSelectedFitness i k = selectedRand + fromIntegral (i - 1) / fromIntegral k
  selectedFitness i k = if unfilteredSelectedFitness i k <= 1
                            then unfilteredSelectedFitness i k
                            else unfilteredSelectedFitness i k - 1

pieFitnessSubselect :: FitnessFunction -> Population -> Double -> Double -> Chromosome
pieFitnessSubselect fitness pop 0 r = error "fitness should never be 0"
pieFitnessSubselect fitness pop t r = fitnessSelection 0 pop where
  fitnessSelection summary (chromosome : chromosomes) =
    let relFitness = fromIntegral (fitness chromosome) / t
      in if (relFitness + summary) > r
        then chromosome
        else fitnessSelection (summary+relFitness) chromosomes

pieOrderSubselect :: Population -> Double -> Double -> Chromosome
pieOrderSubselect pop 0 r = error "fitness should never be 0"
pieOrderSubselect pop t r = orderSelection 0 1 pop where
  orderSelection summary order (chromosome : chromosomes) =
    let relFitness = fromIntegral order / t
      in if (relFitness + summary) > r
        then chromosome
        else orderSelection (summary+relFitness) (order+1) chromosomes

rankingSelection :: SelectionMethod
rankingSelection pop k fitness seed = [pieOrderSubselect pop total (selectRands!!i) | i <- [0..(k-1)]] where
                                        selectRands = fst (randDoubles seed k)
                                        popSize = length pop
                                        total = fromIntegral ( (popSize + 1) * popSize) / 2.0
                                        alterPop = sortOn fitness pop

chromosomeBattle :: Bool -> [Chromosome] -> FitnessFunction -> Seed -> Chromosome
chromosomeBattle isStatistic chromosomes fitness seed = if isStatistic && randDouble seed < 0.2
                                                        then head (sortOn fitness chromosomes)
                                                        else head (sortOn (Data.Ord.Down . fitness) chromosomes)


tournamentSelection :: Bool -> SelectionMethod
tournamentSelection isStochastic pop k fitness seed = [ chromosomeBattle isStochastic (rankingSelection pop 2 fitness (seeds!!(i*2))) fitness (seeds!!(1+i*2))| i <- [0..(k-1)]] where
  seeds = randSeeds seed (k*2)

tournamentDeterministicSelection :: SelectionMethod
tournamentDeterministicSelection = tournamentSelection False

tournamentStochasticSelection :: SelectionMethod
tournamentStochasticSelection  = tournamentSelection True