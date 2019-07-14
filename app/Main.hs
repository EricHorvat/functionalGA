module Main where

import GA
import GABase
import Selection
import Cross
import Mutation
import Replace

import Random
import System.Random
import Control.Monad.Trans.State

sumFitness:: Chromosome -> Double
sumFitness chr = sum (map alleleValue chr)

main :: IO ()
main = do
  stdGen <- newStdGen
  let seedPop = initialPopulation (boundedIntChromosomeGenerator 5 (-150,150)) stdGen 50

  let (s1:s2:s3:s4:sEnd:_) = randSeeds (snd seedPop) 5
  let resultPop = ga 500 (const False) (nextGen eliteSelection 4 cross1point 0.2 mutateMultiGenChromosome 0.1 replaceOld eliteSelection sumFitness) seedPop
  print resultPop

  print (randomSelection resultPop 25 sumFitness sEnd)
  print (rouletteSelection resultPop 25 sumFitness sEnd)

  print (randSeeds s1 5)
  --print (evalState allTypes stdGen)
  --let statessss = execState allTypes stdGen
  --print (evalState allTypes statessss)

boundedIntChromosomeGenerator :: Int -> (Int,Int) -> ChromosomeGenerator
boundedIntChromosomeGenerator l bound seed = mutateMultiGenChromosome 1.0 seed (map (const (BoundedInt bound 0)) [1..l])