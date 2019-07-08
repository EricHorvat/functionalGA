module Selection (
  --select,
  SelectionMethod,
  eliteSelection,
  randomSelection
)where

import Data.Ord
import Data.Sort
import System.Random

import Random
import GABase
import Utils

--select f n (p,s) = ([select_1 f p t ( fromInteger (rs!!i) ) | i<-[0..n-1]] , s1) where
--  (s1:s2:_) = randSeeds s
--  rs = [r `mod` 100 | r <- randIntegers s2]
--  t = sum (map f p) -- total fitness
  -- find an item for which the cumulative fitness exceeds r

select_1 fitness pop 0 r = error "fitness should never be 0"
select_1 fitness pop t r = select_2 0 pop where
  select_2 s (item : rest) =
    let relFitness = (fitness item/t)*100.0
      in if (relFitness + s) > r
        then item
        else select_2 (s+relFitness) rest

-- eliteSelection ; sort then take first k elems
-- roulette ; upper ^
-- ranking ; inverse sort, as roulette where total_fitness = (n + 1) * n / 2 -- see even
-- tournament deterministic; select random 2; compare
-- tournament stochastic; select random 2; invert compare with if
-- universal ; see with detail, should be base in roulette
-- boltzmann ; if active, its a f to the fitness

type SelectionMethod = Population -> Int -> FitnessFunction -> Seed -> [Chromosome]

eliteSelection :: SelectionMethod
eliteSelection pop k fitness seed = take k (sortOn (Data.Ord.Down . fitness) pop ) --HLINT https://github.com/ndmitchell/hlint/blob/master/data/hlint.yaml

randomSelection :: SelectionMethod
randomSelection pop k fitness seed = take k (map snd (sortOn (Data.Ord.Down . fst) [(selectRands!!i, pop!!i) | i <- [0..(popSize-1)]])) where
  selectRands = fst (randInts seed popSize)
  popSize = length pop