module Random (
  Seed,
  randSeeds,
  randInts,
  randInt,
  randBoundedInt,
  randDoubles,
  randDouble
  ) where

import System.Random

type Seed = StdGen

randSeeds :: StdGen -> Int -> [StdGen]
randSeeds g 0 = []
randSeeds g i = randSeeds (snd (split g)) (i-1) ++ [fst (split g)]

randInt :: StdGen -> Int
randInt g = fst (random g)

randBoundedInt :: StdGen -> Int -> Int -> Int
randBoundedInt g min max = fst (randomR (min,max) g)

randDouble :: StdGen -> Double
randDouble g = fst (random g)

randList :: Random a => StdGen -> Int -> ([a], StdGen)
randList g 0 = ([], g)
randList g i = (fst randApplied : fst prev, snd prev) where
  prev = randList (snd randApplied) (i-1)
  randApplied = random g

randInts :: StdGen -> Int -> ([Int], StdGen)
randInts = randList

randDoubles :: StdGen -> Int -> ([Double], StdGen)
randDoubles = randList

--randIntegers :: StdGen -> [Integer]
--randIntegers = randoms

--randIntegers :: StdGen -> Integer -> Integer -> [Integer] --generate random ints
--randIntegers g min max = randomRs (min, max) g

--randDoubles :: StdGen -> Double -> Double -> [Double] --generate random ints
--randDoubles g min max = randomRs (min, max) g