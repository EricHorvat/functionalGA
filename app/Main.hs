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
doubleInt :: Chromosome -> Int
doubleInt chr = alleleIntValue (head chr) + alleleIntValue (head (tail chr))

main = do
  stdGen <- newStdGen
  let seedPop = initialPopulation (const doubleValueAllele) stdGen 50

  let (s1:s2:s3:s4:sEnd:_) = randSeeds (snd seedPop) 5
  let selected = eliteSelection (fst seedPop) 6 (const 1) s1
  let crossed = cross cross1point s2 selected
  print selected
  print crossed
  let iii = length crossed
  let mutated = [mutateChromosome (mutateRands!!i) (crossed!!i) | i <- [0..(iii-1)]] where mutateRands = randSeeds s3 6
  print mutated

  --print (eliteSelection (mutated ++ selected ++ mutated) 6 aaa sEnd)
  print (randomSelection [[allele i] | i  <- [1..10]] 6 aaaa sEnd)

  print (rouletteSelection [[allele i] | i  <- [1..10] ++ [1..5] ++ [1..5]] 6 aaaa sEnd)

  let resultPop = ga 500 (const False) (nextGen eliteSelection 4 cross1point mutateChromosome replaceOld eliteSelection doubleInt) seedPop
  print resultPop
  print (randomSelection resultPop 25 doubleInt sEnd)
  print (rouletteSelection resultPop 25 doubleInt sEnd)

  print (rouletteSelection [[allele i] | i  <- [1..10] ++ [1..5] ++ [1..5]] 6 aaaa sEnd)
  print (rouletteSelection [[allele i] | i  <- [1..10] ++ [1..5] ++ [1..5]] 6 aaaa s1)
  print (rouletteSelection [[allele i] | i  <- [1..10] ++ [8..10] ++ [9..11]] 6 aaaa sEnd)

  print (rankingSelection [[allele i] | i  <- [1..10] ++ [1..5] ++ [1..5]] 6 aaaa sEnd)
  print (rankingSelection [[allele i] | i  <- [1..10] ++ [1..5] ++ [1..5]] 6 aaaa s1)
  print (rankingSelection [[allele i] | i  <- [1..10] ++ [8..10] ++ [9..11]] 6 aaaa sEnd)

  print (tournamentStochasticSelection [[allele i] | i  <- [1..10] ++ [1..5] ++ [1..5]] 6 aaaa sEnd)
  print (tournamentStochasticSelection [[allele i] | i  <- [1..10] ++ [1..5] ++ [1..5]] 6 aaaa s1)
  print (tournamentStochasticSelection [[allele i] | i  <- [1..10] ++ [8..10] ++ [9..11]] 6 aaaa sEnd)

  print (tournamentDeterministicSelection [[allele i] | i  <- [1..10] ++ [1..5] ++ [1..5]] 6 aaaa sEnd)
  print (tournamentDeterministicSelection [[allele i] | i  <- [1..10] ++ [1..5] ++ [1..5]] 6 aaaa s1)
  print (tournamentDeterministicSelection [[allele i] | i  <- [1..10] ++ [8..10] ++ [9..11]] 6 aaaa sEnd)


-- mutateAllele seed (C i) = C (head (fst (randInts seed 5)) `mod` 2 == 0)






-- type FullNextGenerationFunction = SelectionMethod -> Int -> CrossMethod -> MutateMethod -> ReplaceMethod -> SelectionMethod -> FitnessFunction -> NextGenerationFunction




main :: IO ()
--main = putStrLn (if value (head (head resultPop)) then "T" else "F")
