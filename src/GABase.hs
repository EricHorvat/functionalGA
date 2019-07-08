module GABase (
Population,
SeededPopulation,
EndCheckFunction,
Chromosome,
ChromosomeGenerator,
Allele,
Seed,
FitnessFunction,
mutateAllele,
onlyValueAllele,
value,
allele,
alleleIntValue,
doubleValueAllele,
fifValueAllele
) where

import Random
import System.Random

data Allele = A Int | B String | C Bool deriving Show

mutateAllele :: Seed -> Allele -> Allele
mutateAllele seed (A i) = A (head (randomRs (-15,15) seed))
mutateAllele seed (B i) = B " "
mutateAllele seed (C i) = C (head (fst (randInts seed 5)) `mod` 2 == 0)

value :: Allele -> Bool
value (C b) = b

allele :: Int -> Allele
allele = A

alleleIntValue :: Allele -> Int
alleleIntValue (A v) = v

onlyValueAllele :: [Allele]
onlyValueAllele = [C False]

doubleValueAllele :: [Allele]
doubleValueAllele = [A 0 , A 0 ]

fifValueAllele :: [Allele]
--fifValueAllele = [A 0 , A 0 , A 0 , A 0 , A 0 , A 0 , A 0 , A 0 , A 0 , A 0 , A 0 , A 0 , A 0 , A 0 , A 0]
fifValueAllele = [A 0 , A 0 , A 0 , A 0 , A 0 , A 0]

type Chromosome = [Allele]

type Population = [Chromosome]

type SeededPopulation = ([Chromosome] , Seed)

type FitnessFunction = Chromosome -> Int

type ChromosomeGenerator = Seed -> Chromosome

type EndCheckFunction = Population -> Bool