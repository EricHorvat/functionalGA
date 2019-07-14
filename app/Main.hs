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

sumFitness :: FitnessFunction
sumFitness chr = sum (map alleleValue chr)

polynomialRootFitness :: Double -> FitnessFunction
polynomialRootFitness value chr = 1 / (1 + abs (sum (
  zipWith (*)
    (zipWith (**)
      (map (const value) [1..length chr])
      (map fromIntegral [0..(length chr - 1)]))
    (map alleleValue chr)
  )))

main :: IO ()
main = do
  stdGen <- newStdGen
  let fitnessF = polynomialRootFitness 6
  let seedPop = initialPopulation (boundedIntChromosomeGenerator 5 (-150,150)) stdGen 50

  let (s1:s2:s3:s4:sEnd:_) = randSeeds (snd seedPop) 5
  let resultPop = ga 500 (const False) (nextGen eliteSelection 4 cross1point 0.2 mutateMultiGenChromosome 0.1 replaceOld eliteSelection fitnessF) seedPop
  print resultPop

  print (randomSelection resultPop 25 fitnessF sEnd)
  print (rouletteSelection resultPop 25 fitnessF sEnd)

  print (randSeeds s1 5)
  --print (evalState allTypes stdGen)
  --let statessss = execState allTypes stdGen
  --print (evalState allTypes statessss)

boundedIntChromosomeGenerator :: Int -> (Int,Int) -> ChromosomeGenerator
boundedIntChromosomeGenerator l bound seed = mutateMultiGenChromosome 1.0 seed (map (const (BoundedInt bound 0)) [1..l])