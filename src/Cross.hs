module Cross (
  CrossMethod,
  cross,
  cross1point
)where

import Random
import GABase

--crossAll cross cl pCross (p,s) =
--  (cr (randDoubles s1 1) [r `mod` cl | r<-randInts s2] p, s3) where
--    (s1:s2:s3:_) = randSeeds s
--    cr _ _ [] = []
--    cr _ _ [i] = [i]
--    cr (r3:rs3) (r4:rs4) (i1:i2:r)  | r3 > pCross = i1:i2: cr rs3 rs4 r
--                                    | otherwise = cross r4 i1 i2 ++ cr rs3 rs4 r

cross1pointMethod :: Int -> Chromosome -> Chromosome -> [Chromosome]
cross1pointMethod i c1 c2 = (take i c1 ++ drop i c2) : [take i c2 ++ drop i c1]

cross1point :: CrossMethod
cross1point seed = cross1pointMethod ri where (ri:_) = fst (randInts seed 1)

type CrossMethod = Seed -> Chromosome -> Chromosome -> [Chromosome]

cross :: CrossMethod -> Seed -> [Chromosome] -> [Chromosome]
cross cross_method seed [] = []
cross cross_method seed [chromosome] = error "Crossing odd number of chromosomes"
cross cross_method seed (chromosomeA:chromosomeB:chromosomes) =
  cross_method seed1 chromosomeA chromosomeB ++
  cross cross_method seed2 chromosomes where [seed1, seed2] = randSeeds seed 2
