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
randSeeds g i = snd (foldr (\x (y,z) -> (snd (split y), fst (split y) : z)) (g,[]) [1..i])

randInt :: StdGen -> Int
randInt g = fst (random g)

randBoundedInt :: StdGen -> Int -> Int -> Int
randBoundedInt g min max = fst (randomR (min,max) g)

randDouble :: StdGen -> Double
randDouble g = fst (random g)

randList :: Random a => StdGen -> Int -> ([a], StdGen)
randList g i = foldr (\x (y,z) -> (fst (random z) : y, snd (next z))) ([fst (random g)], snd (next g)) [1..i]

randInts :: StdGen -> Int -> ([Int], StdGen)
randInts = randList

randDoubles :: StdGen -> Int -> ([Double], StdGen)
randDoubles = randList