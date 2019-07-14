module MathGA (
  sumFitness,
  polynomialRootFitness,
  boundedIntChromosomeGenerator
) where

import GABase
import Mutation

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

boundedIntChromosomeGenerator :: Int -> (Int,Int) -> ChromosomeGenerator
boundedIntChromosomeGenerator l bound seed = mutateFully seed (map (const (BoundedInt bound 0)) [1..l])