module Cross (
  CrossMethod,
  cross,
  cross1point,
  cross2point,
  anularCross,
  uniformCross
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

cross1pointMethod :: Int -> Chromosome -> Chromosome -> (Chromosome,Chromosome)
cross1pointMethod i c1 c2 = (take i c1 ++ drop i c2, take i c2 ++ drop i c1)

cross2pointMethod :: Int -> Int -> Chromosome -> Chromosome -> (Chromosome,Chromosome)
cross2pointMethod r1 r2 c1 c2 = uncurry (cross1pointMethod r1) partial where
  partial = cross1pointMethod r2 c1 c2

cross1point :: CrossMethod
cross1point seed = cross1pointMethod (randInt seed)

cross2point :: CrossMethod
cross2point seed c1 c2 = uncurry (cross1pointMethod r1) partial where
  (r1:r2:_) = fst (randInts seed 2)
  partial = cross1pointMethod r2 c1 c2

anularCross :: CrossMethod
anularCross seed c1 c2 =  if r + l >= length c1
                          then resultrl
                          else cross2pointMethod r (r+l) c1 c2
                          where
  (r:l:_) = fst (randInts seed 2)
  partial0 = cross1pointMethod 0 c1 c2
  partialrl = uncurry (cross1pointMethod (r + l - length c1)) partial0
  resultrl = uncurry (cross1pointMethod r) partialrl

uniformAlleleCross boolean a1 a2 = if boolean then a1 else a2

uniformCross :: CrossMethod
uniformCross seed c1 c2 = (map (uncurry (uncurry uniformAlleleCross)) boolAllelesTuple1,
                           map (uncurry (uncurry uniformAlleleCross)) boolAllelesTuple2) where
  booleans = map (> 0.5) (fst (randDoubles seed (length c1)))
  boolAllelesTuple1 = zip (zip booleans c1) c2
  boolAllelesTuple2 = zip (zip booleans c2) c1

type CrossMethod = Seed -> Chromosome -> Chromosome -> (Chromosome,Chromosome)

cross :: CrossMethod -> Seed -> Double -> [Chromosome] -> [Chromosome]
cross cross_method seed pCross [] = []
cross cross_method seed pCross [chromosome] = error "Crossing odd number of chromosomes"
cross cross_method seed pCross (chromosomeA:chromosomeB:chromosomes) =
  (if randDouble seed3 > pCross
  then [resultChromosomeA,resultChromosomeB]
  else [chromosomeA,chromosomeB] ) ++
  cross cross_method seed2 pCross chromosomes where
    [seed1, seed2, seed3] = randSeeds seed 3
    (resultChromosomeA, resultChromosomeB) = cross_method seed1 chromosomeA chromosomeB
