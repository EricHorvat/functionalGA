module Selection (
  SelectionMethod,
  eliteSelection,
  randomSelection,
  rouletteSelection,
  rankingSelection,
  universalSelection,
  tournamentDeterministicSelection,
  tournamentStochasticSelection
  ) where

import Data.Ord
import Data.Sort
import System.Random

import Random
import GABase

type SelectionMethod = Population -> Int -> FitnessFunction -> Seed -> [Chromosome]

eliteSelection :: SelectionMethod
eliteSelection pop k fitness _ = take k (sortOn (Data.Ord.Down . fitness) pop )
--HLINT https://github.com/ndmitchell/hlint/blob/master/data/hlint.yaml

randomSelection :: SelectionMethod
randomSelection pop k _ seed = take k (map snd (sortOn fst seededPop)) where
  selectRands = fst (randInts seed popSize)
  popSize = length pop
  seededPop = zip selectRands pop

pieFitnessSubselect :: FitnessFunction -> Population -> Double -> Double -> Chromosome
pieFitnessSubselect fitness pop 0 r = error "fitness should never be 0"
pieFitnessSubselect fitness pop t r = fitnessSelection 0 pop where
  fitnessSelection summary (chromosome : chromosomes) =
    let relFitness = fitness chromosome / t
      in if (relFitness + summary) > r
        then chromosome
        else fitnessSelection (summary+relFitness) chromosomes

rouletteSelection :: SelectionMethod
rouletteSelection pop k fitness seed = map (pieFitnessSubselect fitness pop total) selectRands where
  selectRands = fst (randDoubles seed k)
  popSize = length pop
  total = sum (map fitness pop)

universalSelection :: SelectionMethod
universalSelection pop k fitness seed = map (pieFitnessSubselect fitness pop total . selectedFitness) [0..(k-1)] where
  selectedRand = randDouble seed
  popSize = length pop
  total = sum (map fitness pop)
  unfilteredSelectedFitness i = selectedRand + fromIntegral (i - 1) / fromIntegral k
  selectedFitness i = if unfilteredSelectedFitness i <= 1
                            then unfilteredSelectedFitness i
                            else unfilteredSelectedFitness i - 1

pieOrderSubselect :: Population -> Double -> Double -> Chromosome
pieOrderSubselect pop 0 r = error "fitness should never be 0"
pieOrderSubselect pop t r = orderSelection 0 1 pop where
  orderSelection summary order (chromosome : chromosomes) =
    let relFitness = fromIntegral order / t
      in if (relFitness + summary) > r
        then chromosome
        else orderSelection (summary+relFitness) (order+1) chromosomes

rankingSelection :: SelectionMethod
rankingSelection pop k fitness seed = map (pieOrderSubselect alterPop total) selectRands where
                                        selectRands = fst (randDoubles seed k)
                                        popSize = length pop
                                        total = fromIntegral ( (popSize + 1) * popSize) / 2.0
                                        alterPop = sortOn fitness pop

chromosomeBattle :: Bool -> [Chromosome] -> FitnessFunction -> Seed -> Chromosome
chromosomeBattle isStatistic chromosomes fitness seed = if isStatistic && randDouble seed < 0.2
                                                        then head (sortOn fitness chromosomes)
                                                        else head (sortOn (Data.Ord.Down . fitness) chromosomes)


tournamentSelection :: Bool -> SelectionMethod
tournamentSelection isStochastic pop k fitness seed = map battle tupledSeeds where
  battle (seed1,seed2) = chromosomeBattle isStochastic (randomSelection pop 2 fitness seed1) fitness seed2
  allSeeds = randSeeds seed (k*2)
  tupledSeeds = zip (take k allSeeds) (drop k allSeeds)

tournamentDeterministicSelection :: SelectionMethod
tournamentDeterministicSelection = tournamentSelection False

tournamentStochasticSelection :: SelectionMethod
tournamentStochasticSelection  = tournamentSelection True

multipleSelection :: [(SelectionMethod, Double)] -> SelectionMethod
multipleSelection tuples pop k fitness seed =
  concatMap (\(f,x)-> f x) (zip (map (uncurry apply) tuples) seeds) where
    apply selectionMethod percentage = selectionMethod pop (floor(fromIntegral k * percentage)) fitness
    seeds = randSeeds seed (length tuples)


boltzmann :: Int -> FitnessFunction -> FitnessFunction
boltzmann iteration fitness chromosome = (1000.0 / fromIntegral iteration + 1.0) * fitness chromosome + 1.0