module Replace (
  replaceOld,
  replaceNewOld
  ) where

import GABase
import Selection

replaceOld :: ReplaceMethod
replaceOld pop chromosomes seed select_method fitness =
  select_method pop (length pop - length chromosomes) fitness seed ++ chromosomes

replaceNewOld :: ReplaceMethod
replaceNewOld pop chromosomes seed select_method fitness =
  select_method pop (length pop - length chromosomes) fitness seed
  ++ select_method (pop ++ chromosomes) (length chromosomes) fitness seed
