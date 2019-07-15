module GABase (
  Population,
  SeededPopulation,
  EndCheckFunction,
  Chromosome,
  ChromosomeGenerator,
  Seed,
  FitnessFunction,
  SelectionMethod,
  CrossMethod,
  MutateMethod,
  ReplaceMethod,
  mutateAllele,
  alleleValue,
  Allele (BoundedInt, BoundedDouble, Vestment, EndCheck, SelectionAllele, CrossAllele, MutateAllele, ReplaceAllele, FitnessAllele, GeneratorAllele, SeedAllele),
  VestmentItem
  ) where

import Random
import System.Random

type SelectionMethod = Population -> Int -> FitnessFunction -> Seed -> [Chromosome]

type CrossMethod = Seed -> Chromosome -> Chromosome -> (Chromosome,Chromosome)

type MutateMethod = Double -> Seed -> Chromosome -> Chromosome

type ReplaceMethod =
  Population ->
  [Chromosome] ->
  Seed ->
  SelectionMethod ->
  FitnessFunction ->
  Population

type VestmentItem = [Double]

data Allele = BoundedInt (Int,Int) Int      |
  BoundedDouble (Double,Double) Double      |
  Vestment [VestmentItem] Int               |
  EndCheck [EndCheckFunction] Int           |
  SelectionAllele [SelectionMethod] Int     |
  CrossAllele [CrossMethod] Int             |
  MutateAllele [MutateMethod] Int           |
  ReplaceAllele [ReplaceMethod] Int         |
  FitnessAllele [FitnessFunction] Int       |
  GeneratorAllele [ChromosomeGenerator] Int |
  SeedAllele Seed

instance Show Allele where
  show (BoundedInt _ v) = show v
  show (BoundedDouble _ v) = show v
  show (Vestment _ v) = show v
  show (EndCheck _ v) = show v
  show (SelectionAllele _ v) = show v
  show (CrossAllele _ v) = show v
  show (MutateAllele _ v) = show v
  show (ReplaceAllele _ v) = show v
  show (FitnessAllele _ v) = show v
  show (GeneratorAllele _ v) = show v
  show (SeedAllele v) = show v

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
mutateAllele seed (EndCheck values _) = EndCheck values (randBoundedInt seed (0,length values - 1))
mutateAllele seed (SelectionAllele values _) = SelectionAllele values (randBoundedInt seed (0,length values - 1))
mutateAllele seed (CrossAllele values _) = CrossAllele values (randBoundedInt seed (0,length values - 1))
mutateAllele seed (MutateAllele values _) = MutateAllele values (randBoundedInt seed (0,length values - 1))
mutateAllele seed (ReplaceAllele values _) = ReplaceAllele values (randBoundedInt seed (0,length values - 1))
mutateAllele seed (FitnessAllele values _) = FitnessAllele values (randBoundedInt seed (0,length values - 1))
mutateAllele seed (GeneratorAllele values _) = GeneratorAllele values (randBoundedInt seed (0,length values - 1))
mutateAllele seed (SeedAllele _) = SeedAllele (snd (split seed))

alleleValue :: Allele -> Double
alleleValue (BoundedInt _ v) = fromIntegral v
alleleValue (BoundedDouble _ v) = v

alleleValues :: Allele -> [Double]
alleleValues (Vestment values index) = values!!index