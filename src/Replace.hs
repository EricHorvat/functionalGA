module Replace (
  ReplaceMethod,
  replaceOld,
  replaceNewOld
  ) where

import GABase
import Selection

type ReplaceMethod = Population -> [Chromosome] -> Seed -> SelectionMethod -> FitnessFunction -> Population

basicReplace :: ReplaceMethod
basicReplace pop chromosomes seed select_method fitness =
  select_method pop (length pop - length chromosomes) fitness seed ++ chromosomes

replaceOld :: ReplaceMethod
replaceOld = basicReplace

replaceNewOld :: ReplaceMethod
replaceNewOld pop chromosomes seed select_method fitness =
  select_method pop (length pop - length chromosomes) fitness seed
  ++ select_method (pop ++ chromosomes) (length chromosomes) fitness seed
