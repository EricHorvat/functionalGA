module GABase (
  Population,
  SeededPopulation,
  EndCheckFunction,
  Chromosome,
  ChromosomeGenerator,
  Seed,
  FitnessFunction,
  mutateAllele,
  onlyValueAllele,
  doubleValueAllele,
  fifValueAllele,
  alleleValue,
  Allele (BoundedInt, B, C)
  ) where

import Random
import System.Random

data Allele = BoundedInt (Int,Int) Int | B String | C Bool deriving Show

type Chromosome = [Allele]

type Population = [Chromosome]

type SeededPopulation = (Population , Seed)

type FitnessFunction = Chromosome -> Double

type ChromosomeGenerator = Seed -> Chromosome

type EndCheckFunction = Population -> Bool

mutateAllele :: Seed -> Allele -> Allele
mutateAllele seed (BoundedInt bound i) = BoundedInt bound (head (randomRs bound seed))
mutateAllele seed (B i) = B " "
mutateAllele seed (C i) = C (head (fst (randInts seed 5)) `mod` 2 == 0)

alleleValue :: Allele -> Double
alleleValue (BoundedInt _ v) = fromIntegral v
alleleValue (B i) = 6
alleleValue (C b) = 5

onlyValueAllele :: [Allele]
onlyValueAllele = [C False]

doubleValueAllele :: [Allele]
doubleValueAllele = [BoundedInt (-15,15) 0 , BoundedInt (-15,15) 0 ]

fifValueAllele :: [Allele]
fifValueAllele = [BoundedInt (-15,15) 0 , BoundedInt (-15,15) 0 , BoundedInt (-15,15) 0 , BoundedInt (-15,15) 0 , BoundedInt (-15,15) 0 , BoundedInt (-15,15) 0]