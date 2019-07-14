module GABase (
  Population,
  SeededPopulation,
  EndCheckFunction,
  Chromosome,
  ChromosomeGenerator,
  Seed,
  FitnessFunction,
  mutateAllele,
  alleleValue,
  Allele (BoundedInt, BoundedDouble, Vestment)
  ) where

import Random
import System.Random

data Allele = BoundedInt (Int,Int) Int | BoundedDouble (Double,Double) Double | Vestment [[Double]] Int

instance Show Allele where
  show (BoundedInt bound v) = show v
  show (BoundedDouble bound v) = show v
  show (Vestment files v) = show v

type Chromosome = [Allele]

type Population = [Chromosome]

type SeededPopulation = (Population , Seed)

type FitnessFunction = Chromosome -> Double

type ChromosomeGenerator = Seed -> Chromosome

type EndCheckFunction = Population -> Bool

mutateAllele :: Seed -> Allele -> Allele
mutateAllele seed (BoundedInt bound _) = BoundedInt bound (head (randomRs bound seed))
mutateAllele seed (BoundedDouble (min,max) _) = BoundedDouble (min,max) (min + randDouble seed * (max - min))
mutateAllele seed (Vestment values _) = Vestment values (randBoundedInt seed (0,length values - 1))

alleleValue :: Allele -> Double
alleleValue (BoundedInt _ v) = fromIntegral v
alleleValue (BoundedDouble _ v) = v

alleleValues :: Allele -> [Double]
alleleValues (Vestment values index) = values!!index