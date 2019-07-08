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

--randomSeeds g :: [RandomGen g]

--randomSeeds g = ran

aaa b = if value (head b) then 21 else 2
aaaa a = alleleIntValue (head a)
--doubleInt :: Chromosome -> Int
--doubleInt chr = alleleIntValue (head chr) + alleleIntValue (head (tail chr))

multipleInt:: Chromosome -> Int
multipleInt chr = sum (map alleleIntValue chr)

main = do
  stdGen <- newStdGen
  let seedPop = initialPopulation (const fifValueAllele) stdGen 50

  let (s1:s2:s3:s4:sEnd:_) = randSeeds (snd seedPop) 5
  let selected = eliteSelection (fst seedPop) 6 (const 1) s1
  let crossed = cross cross1point s2 selected
  print selected
  print crossed
  let iii = length crossed
  let mutated = [mutateChromosome (mutateRands!!i) (crossed!!i) | i <- [0..(iii-1)]] where mutateRands = randSeeds s3 6
  print "ASDASDASDADASDADASDADSADADASDADSAD"
  print mutated
  let crossed2 = cross cross1point s2 mutated
  print crossed2
  let crossed3 = cross cross2point s2 mutated
  print crossed3
  let crossed4 = cross anularCross s2 mutated
  print crossed4
  let crossed5 = cross uniformCross s2 mutated
  print crossed5
  print "ASDASDASDADASDADASDADSADADASDADSAD"

  let resultPop = ga 500 (const False) (nextGen eliteSelection 4 cross1point mutateChromosome replaceOld eliteSelection multipleInt) seedPop
  print resultPop


  let resultPop2 = ga 500 (const False) (nextGen eliteSelection 4 cross2point mutateChromosome replaceOld eliteSelection multipleInt) seedPop
  print resultPop2

  let resultPop3 = ga 500 (const False) (nextGen eliteSelection 4 anularCross mutateChromosome replaceOld eliteSelection multipleInt) seedPop
  print resultPop3

  let resultPop4 = ga 500 (const False) (nextGen eliteSelection 4 uniformCross mutateChromosome replaceOld eliteSelection multipleInt) seedPop
  print resultPop4
  print (randomSelection resultPop 25 multipleInt sEnd)
  print (randomSelection resultPop2 25 multipleInt sEnd)
  print (randomSelection resultPop3 25 multipleInt sEnd)
  print (randomSelection resultPop4 25 multipleInt sEnd)
  print (rouletteSelection resultPop 25 multipleInt sEnd)
  print (rouletteSelection resultPop2 25 multipleInt sEnd)
  print (rouletteSelection resultPop3 25 multipleInt sEnd)
  print (rouletteSelection resultPop4 25 multipleInt sEnd)


-- mutateAllele seed (C i) = C (head (fst (randInts seed 5)) `mod` 2 == 0)






-- type FullNextGenerationFunction = SelectionMethod -> Int -> CrossMethod -> MutateMethod -> ReplaceMethod -> SelectionMethod -> FitnessFunction -> NextGenerationFunction




main :: IO ()
--main = putStrLn (if value (head (head resultPop)) then "T" else "F")
