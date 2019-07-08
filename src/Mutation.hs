module Mutation (
  mutate,
  MutateMethod,
  mutateMultiGenChromosome
)where

import Random
import GABase

--mutate mutOp (p,s) = ([mutOp chrom (rs!!i) | (i,chrom)<- zip [1..] p],rs!!0) where rs = randSeeds s (length p)

type MutateMethod = Seed -> Double -> Chromosome -> Chromosome

mutateMultiGenChromosome :: MutateMethod
mutateMultiGenChromosome seed pMutation chromosome =
  [if pMutation > randDouble (mutateRands!!(i*2))
   then mutateAllele (mutateRands!!((i * 2) + 1)) (chromosome!!i)
   else chromosome!!i | i <- [0..(k-1)]] where
    mutateRands = randSeeds seed (k * 2)
    k = length chromosome

mutateGenChromosome :: MutateMethod
mutateGenChromosome seed pMutation chromosome =
  if pMutation > randDouble seed1
  then [if i == index then mutateAllele seed2 (chromosome!!i) else chromosome!!i | i <- [0..(k - 1)]]
  else chromosome where
    (seed1:seed2:seed3:_) = randSeeds seed 3
    index = randBoundedInt seed3 0 (k - 1)
    k = length chromosome

mutate :: MutateMethod -> Seed -> Double -> [Chromosome] -> [Chromosome]
mutate mutateMethod seed pMutation chromosomes =
  [mutateMethod (mutateRands!!i) pMutation (chromosomes!!i) | i <- [0..(k-1)]] where
    mutateRands = randSeeds seed k
    k = length chromosomes
