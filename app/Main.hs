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
  let crossed = cross cross1point s2 0.2 selected
  print selected
  print crossed
  let iii = length crossed
  let mutated = [mutateMultiGenChromosome (mutateRands!!i) 0.1 (crossed!!i) | i <- [0..(iii-1)]] where mutateRands = randSeeds s3 6
  print "ASDASDASDADASDADASDADSADADASDADSAD"
  print mutated
  let crossed2 = cross cross1point s2 0.2 mutated
  print crossed2
  print "ASDASDASDADASDADASDADSADADASDADSAD"

  let resultPop = ga 500 (const False) (nextGen eliteSelection 4 cross1point 0.2 mutateMultiGenChromosome 0.1 replaceOld eliteSelection multipleInt) seedPop
  print resultPop

  print (randomSelection resultPop 25 multipleInt sEnd)
  print (rouletteSelection resultPop 25 multipleInt sEnd)


-- mutateAllele seed (C i) = C (head (fst (randInts seed 5)) `mod` 2 == 0)






-- type FullNextGenerationFunction = SelectionMethod -> Int -> CrossMethod -> MutateMethod -> ReplaceMethod -> SelectionMethod -> FitnessFunction -> NextGenerationFunction




main :: IO ()
--main = putStrLn (if value (head (head resultPop)) then "T" else "F")
