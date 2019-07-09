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
eliteSelection pop k fitness seed = take k (sortOn (Data.Ord.Down . fitness) pop ) --HLINT https://github.com/ndmitchell/hlint/blob/master/data/hlint.yaml

randomSelection :: SelectionMethod
randomSelection pop k fitness seed = take k (map snd (sortOn (Data.Ord.Down . fst) seededPop)) where
  selectRands = fst (randInts seed popSize)
  popSize = length pop
  seededPop = zip selectRands pop

rouletteSelection :: SelectionMethod
rouletteSelection pop k fitness seed = map (pieFitnessSubselect fitness pop total) selectRands where
  selectRands = fst (randDoubles seed k)
  popSize = length pop
  total = fromIntegral (sum (map fitness pop))

universalSelection :: SelectionMethod
universalSelection pop k fitness seed = map (pieFitnessSubselect fitness pop total . selectedFitness) [0..(k-1)] where
  selectedRand = randDouble seed
  popSize = length pop
  total = fromIntegral (sum (map fitness pop))
  unfilteredSelectedFitness i = selectedRand + fromIntegral (i - 1) / fromIntegral k
  selectedFitness i = if unfilteredSelectedFitness i <= 1
                            then unfilteredSelectedFitness i
                            else unfilteredSelectedFitness i - 1

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
rankingSelection pop k fitness seed = map (pieOrderSubselect pop total) selectRands where
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
  battle (seed1,seed2) = chromosomeBattle isStochastic (rankingSelection pop 2 fitness seed1) fitness seed2
  allSeeds = randSeeds seed (k*2)
  tupledSeeds = zip (take k allSeeds) (drop k allSeeds)

tournamentDeterministicSelection :: SelectionMethod
tournamentDeterministicSelection = tournamentSelection False

tournamentStochasticSelection :: SelectionMethod
tournamentStochasticSelection  = tournamentSelection True