module Mutation (
  mutate,
  MutateMethod,
  mutateChromosome
)where

import Random
import GABase

--mutate mutOp (p,s) = ([mutOp chrom (rs!!i) | (i,chrom)<- zip [1..] p],rs!!0) where rs = randSeeds s (length p)

type MutateMethod = Seed -> Chromosome -> Chromosome

mutateChromosome :: MutateMethod
mutateChromosome seed chromosome = [mutateAllele (mutateRands!!i) (chromosome!!i) | i <- [0..(k-1)]] where
                          mutateRands = randSeeds seed k
                          k = length chromosome

mutate :: MutateMethod -> Seed -> [Chromosome] -> [Chromosome]
mutate mutateMethod seed chromosomes = [mutateMethod (mutateRands!!i) (chromosomes!!i) | i <- [0..(k-1)]] where
  mutateRands = randSeeds seed k
  k = length chromosomes
