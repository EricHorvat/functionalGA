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

randSeeds :: Seed -> Int -> [Seed]
randSeeds g i =
  snd (
    foldr (\x (y,z) -> (snd (split y), fst (split y) : z))
    (g,[])
    [1..i]
    )

randInt :: Seed -> Int
randInt g = fst (random g)

randBoundedInt :: Seed -> (Int,Int) -> Int
randBoundedInt g bound = fst (randomR bound g)

randDouble :: Seed -> Double
randDouble g = fst (random g)

randList :: Random a => Seed -> Int -> ([a], Seed)
randList g i =
  foldr (\x (y,z) -> (fst (random z) : y, snd (split z)))
    ([fst (random g)], snd (split g)) [1..i-1]

randInts :: Seed -> Int -> ([Int], Seed)
randInts = randList

randDoubles :: Seed -> Int -> ([Double], Seed)
randDoubles = randList