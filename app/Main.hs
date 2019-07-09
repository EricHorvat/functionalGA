module Main where

import Lib
import GA
import GABase
import Selection
import Cross
import Mutation
import Replace




import Random
import System.Random

multipleInt:: Chromosome -> Int
multipleInt chr = sum (map alleleIntValue chr)

main :: IO ()
main = do
  stdGen <- newStdGen
  let seedPop = initialPopulation (const fifValueAllele) stdGen 50

  let (s1:s2:s3:s4:sEnd:_) = randSeeds (snd seedPop) 5
  let resultPop = ga 500 (const False) (nextGen eliteSelection 4 cross1point 0.2 mutateMultiGenChromosome 0.1 replaceOld eliteSelection multipleInt) seedPop
  print resultPop

  print (randomSelection resultPop 25 multipleInt sEnd)
  print (rouletteSelection resultPop 25 multipleInt sEnd)

