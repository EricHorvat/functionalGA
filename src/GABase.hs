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

type Chromosome = [Allele]

type Population = [Chromosome]

type SeededPopulation = (Population , Seed)

type FitnessFunction = Chromosome -> Int

type ChromosomeGenerator = Seed -> Chromosome

type EndCheckFunction = Population -> Bool

mutateAllele :: Seed -> Allele -> Allele
mutateAllele seed (A i) = A (head (randomRs (-1500,1500) seed))
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