module GAinGA (
  gaFitness,
  gaChromosomeGenerator
) where

import GA
import GABase
import Selection
import Cross
import Mutation
import Replace
import MathGA

import System.Random

import Data.Maybe

gaChromosome :: Chromosome
gaChromosome = [
                 BoundedInt (2,4) 0,
                 EndCheck [const False, const False] 0,
                 SelectionAllele [eliteSelection, tournamentStochasticSelection] 0,
                 BoundedInt (5 , 250) 5,
                 CrossAllele [cross1point, cross2point] 0,
                 BoundedDouble (0.0, 1.0) 0.5,
                 MutateAllele [mutateMultiGenChromosome, mutateGenChromosome] 0,
                 BoundedDouble (0.0, 1.0) 0.5,
                 ReplaceAllele [replaceOld, replaceNewOld] 0,
                 SelectionAllele [eliteSelection, tournamentStochasticSelection] 0,
                 SeedAllele (mkStdGen 0),
                 BoundedInt (500,2500) 0
               ]

gaFitness :: FitnessFunction
gaFitness chr = fitnessF best where
  nextGenF = nextGen (selectionMethod chr)
                     (k chr)
                     (crossMethod chr)
                     (crossProb chr)
                     (mutateMethod chr)
                     (mutateProb chr)
                     (replaceMethod chr)
                     (selectionReplaceMethod chr)
                     fitnessF
  initialPop = initialPopulation (boundedIntChromosomeGenerator 10 (-150,150)) (seedd chr) (populationCount chr)
  lastPop = ga (iterations chr) (endCheckFunction chr) nextGenF initialPop
  best = head (eliteSelection lastPop 1 fitnessF (mkStdGen 0))
  fitnessF = polynomialRootFitness 25

gaChromosomeGenerator :: ChromosomeGenerator
gaChromosomeGenerator seed = mutateFully seed gaChromosome

iterations :: Chromosome -> Int
iterations chromosome =  floor (alleleValue (head chromosome))

endCheckFunction :: Chromosome -> EndCheckFunction
endCheckFunction chromosome = getF (chromosome!!1) where
  getF (EndCheck values v) = values!!v

selectionMethod :: Chromosome -> SelectionMethod
selectionMethod chromosome = getF (chromosome!!2) where
  getF (SelectionAllele values v) = values!!v

k :: Chromosome -> Int
k chromosome =  floor (alleleValue (chromosome!!3)) * 2

crossMethod :: Chromosome -> CrossMethod
crossMethod chromosome = getF (chromosome!!4) where
  getF (CrossAllele values v) = values!!v

crossProb :: Chromosome -> Double
crossProb chromosome =  alleleValue (chromosome!!5)

mutateMethod :: Chromosome -> MutateMethod
mutateMethod chromosome = getF (chromosome!!6) where
  getF (MutateAllele values v) = values!!v

mutateProb :: Chromosome -> Double
mutateProb chromosome =  alleleValue (chromosome!!7)

replaceMethod :: Chromosome -> ReplaceMethod
replaceMethod chromosome = getF (chromosome!!8) where
  getF (ReplaceAllele values v) = values!!v

selectionReplaceMethod :: Chromosome -> SelectionMethod
selectionReplaceMethod chromosome = getF (chromosome!!9) where
  getF (SelectionAllele values v) = values!!v

seedd :: Chromosome -> Seed
seedd chromosome = getSeed (chromosome!!10) where
  getSeed (SeedAllele v) = v

populationCount :: Chromosome -> Int
populationCount chromosome =  floor (alleleValue (chromosome!!11))