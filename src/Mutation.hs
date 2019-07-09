module Mutation (
  mutate,
  MutateMethod,
  mutateMultiGenChromosome
)where

import Random
import GABase

type MutateMethod = Double -> Seed -> Chromosome -> Chromosome

mutateWithProbability pMutation allele seed1 seed2 = if pMutation > randDouble seed1
                                                         then mutateAllele seed2 allele
                                                         else allele

mutateMultiGenChromosome :: MutateMethod
mutateMultiGenChromosome pMutation seed chromosome = map (uncurry(uncurry(mutateWithProbability pMutation))) chromosomeAndSeedTuple where
    mutateRands = randSeeds seed (k * 2)
    k = length chromosome
    chromosomeAndSeedTuple = zip (zip chromosome (take k mutateRands)) (drop k mutateRands)

mutateGenChromosome :: MutateMethod
mutateGenChromosome pMutation seed chromosome =
  if pMutation > randDouble seed1
  then take index chromosome ++ mutateAllele seed2 (chromosome!!index) : drop (index + 1) chromosome
  else chromosome where
    (seed1:seed2:seed3:_) = randSeeds seed 3
    index = randBoundedInt seed3 0 (k - 1)
    k = length chromosome

mutate :: MutateMethod -> Double -> Seed -> [Chromosome] -> [Chromosome]
mutate mutateMethod pMutation seed chromosomes = map (uncurry (mutateMethod pMutation)) chromosomesSeedTuple where
    mutateRands = randSeeds seed k
    chromosomesSeedTuple = zip mutateRands chromosomes
    k = length chromosomes
