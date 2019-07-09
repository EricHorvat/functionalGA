module GA (
initialPopulation,
ga,
nextGen
)where

import GABase
import Random
import Selection
import Cross
import Mutation
import Replace

type NextGenerationFunctionGenerator = SelectionMethod -> Int -> CrossMethod -> Double ->
  MutateMethod -> Double -> ReplaceMethod -> SelectionMethod -> FitnessFunction -> NextGenerationFunction
type NextGenerationFunction = SeededPopulation -> SeededPopulation

ga :: Int -> EndCheckFunction -> NextGenerationFunction -> SeededPopulation -> Population
ga iterations endCheck nextGen (pop,s) | iterations==0 = pop
                                       | endCheck pop = pop
                                       | otherwise = ga (iterations - 1) endCheck nextGen (nextGen (pop,s))

initialPopulation :: ChromosomeGenerator -> Seed -> Int -> SeededPopulation
initialPopulation chromosomeGenerator seed popSize = (map chromosomeGenerator (tail rs), head rs) where rs = randSeeds seed (popSize + 1)

nextGen :: NextGenerationFunctionGenerator
nextGen selectMethod k crossMethod pCross mutateMethod pMutation replaceMethod selectMethod4replace fitness (population, seed) =
  (replaceMethod population mutated s4 selectMethod4replace fitness, sEnd) where
    mutated = mutate mutateMethod pMutation s3 crossed
    crossed = cross crossMethod s2 pCross selected
    selected = selectMethod population k fitness s1
    (s1:s2:s3:s4:sEnd:_) = randSeeds seed 5
