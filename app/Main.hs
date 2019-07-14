module Main where

import GA
import GABase
import Selection
import Cross
import Mutation
import Replace

import Random
import Character

import Control.Monad.Trans.State
import Data.CSV
import Data.String
import Data.Either
import System.Random
import Text.ParserCombinators.Parsec

sumFitness :: FitnessFunction
sumFitness chr = sum (map alleleValue chr)

polynomialRootFitness :: Double -> FitnessFunction
polynomialRootFitness value chr = 1 / (1 + abs (sum (
  zipWith (*)
    (zipWith (**)
      (map (const value) [1..length chr])
      (map fromIntegral [0..(length chr - 1)]))
    (map alleleValue chr)
  )))

main :: IO ()
main = mainCharacter

mainCharacter :: IO ()
mainCharacter = do
  let filenames = ["./res/armas.csv", "./res/botas.csv", "./res/cascos.csv", "./res/guantes.csv", "./res/pecheras.csv"]
  fileData <- mapM (parseFromFile csvFile) filenames
  stdGen <- newStdGen
  characterGA fileData stdGen

characterGA :: [ Either ParseError [[String]]] -> StdGen -> IO ()
characterGA fileData stdGen = if map lefts [fileData] == [[]]
  then
    --print (map fitnessF (fst seedPop))
    print (map fitnessF resultPop)
  else
    print (map lefts [fileData])
  where
    parsedData = map( map(map(read :: String->Double)).fromRight [["ERROR"]]) fileData
    chromosomeGenerator = characterChromosomeGenerator parsedData
    fitnessF = warriorFitness
    seedPop = initialPopulation chromosomeGenerator stdGen 2000
    (s1:s2:s3:s4:sEnd:_) = randSeeds (snd seedPop) 5
    resultPop = ga 500 (const False) (nextGen eliteSelection 10 cross1point 0.2 mutateMultiGenChromosome 0.1 replaceOld eliteSelection fitnessF) seedPop


main2 :: IO ()
main2 = do
  stdGen <- newStdGen
  --let fitnessF = polynomialRootFitness 6
  let fitnessF = polynomialRootFitness 6

  let chromosomeGenerator = boundedIntChromosomeGenerator 5 (-150,150)


  let seedPop = initialPopulation chromosomeGenerator stdGen 50
  let (s1:s2:s3:s4:sEnd:_) = randSeeds (snd seedPop) 5
  let resultPop = ga 500 (const False) (nextGen eliteSelection 4 cross1point 0.2 mutateMultiGenChromosome 0.1 replaceOld eliteSelection fitnessF) seedPop
  print resultPop

  print (randomSelection resultPop 25 fitnessF sEnd)
  print (rouletteSelection resultPop 25 fitnessF sEnd)

  print (randSeeds s1 5)
  --print (evalState allTypes stdGen)
  --let statessss = execState allTypes stdGen
  --print (evalState allTypes statessss)

boundedIntChromosomeGenerator :: Int -> (Int,Int) -> ChromosomeGenerator
boundedIntChromosomeGenerator l bound seed = mutateMultiGenChromosome 1.0 seed (map (const (BoundedInt bound 0)) [1..l])

characterChromosomeGenerator :: [[[Double]]] -> ChromosomeGenerator
characterChromosomeGenerator values seed = mutateMultiGenChromosome 1.0 seed ( BoundedDouble (1.3,2.0) 0.0 : map (flip Vestment 0) values)

