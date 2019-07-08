module Random (
  Seed,
  randSeeds,
  randInts,
  randInt
  ) where

import System.Random

type Seed = StdGen

randSeeds :: StdGen -> Int -> [StdGen]
randSeeds g 0 = []
randSeeds g i = randSeeds (snd (split g)) (i-1) ++ [fst (split g)]

randInts :: StdGen -> Int -> ([Int], StdGen)
randInts = randList

randInt :: StdGen -> Int
randInt g = fst (random g)

randDoubles :: StdGen -> Int -> ([Double], StdGen)
randDoubles = randList

randList :: Random a => StdGen -> Int -> ([a], StdGen)
randList g 0 = ([], g)
randList g i = (fst randApplied : fst prev, snd prev) where
  prev = randList (snd randApplied) (i-1)
  randApplied = random g

randBoundedList :: (Random a , Bounded a) => StdGen -> a -> a -> Int -> ([a], StdGen)
randBoundedList g min max 0 = ([], g)
randBoundedList g min max i = (fst randApplied : fst prev, snd prev) where
  prev = randBoundedList (snd randApplied) min max (i-1)
  randApplied = randomR (min, max) g

--randIntegers :: StdGen -> [Integer]
--randIntegers = randoms

--randIntegers :: StdGen -> Integer -> Integer -> [Integer] --generate random ints
--randIntegers g min max = randomRs (min, max) g

--randDoubles :: StdGen -> Double -> Double -> [Double] --generate random ints
--randDoubles g min max = randomRs (min, max) g