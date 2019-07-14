module Main where

import GA
import GABase
import Selection
import Cross
import Mutation
import Replace

import Character
import GAinGA
import MathGA
import Random

import Control.Monad.Trans.State
import Data.CSV
import Data.String
import Data.Either
import System.Random
import Text.ParserCombinators.Parsec

import Data.Time

main :: IO ()
main = do
  start <- getCurrentTime
  mainGA
  stop <- getCurrentTime
  print $ diffUTCTime stop start

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


mainMath :: IO ()
mainMath = do
  stdGen <- newStdGen
  let fitnessF = polynomialRootFitness 6
  let chromosomeGenerator = boundedIntChromosomeGenerator 5 (-150,150)
  let seedPop = initialPopulation chromosomeGenerator stdGen 50
  let (s1:s2:s3:s4:sEnd:_) = randSeeds (snd seedPop) 5
  let resultPop = ga 500 (const False) (nextGen eliteSelection 4 cross1point 0.2 mutateMultiGenChromosome 0.1 replaceOld eliteSelection fitnessF) seedPop
  print resultPop

  print (randomSelection resultPop 25 fitnessF sEnd)
  print (rouletteSelection resultPop 25 fitnessF sEnd)

  print (randSeeds s1 5)

mainGA :: IO ()
mainGA = do
  stdGen <- newStdGen
  let seedPop = initialPopulation gaChromosomeGenerator stdGen 5
  let (s1:s2:s3:s4:sEnd:_) = randSeeds (snd seedPop) 5
  let resultPop = ga 5 (const False) (nextGen eliteSelection 4 cross1point 0.2 mutateMultiGenChromosome 0.1 replaceOld eliteSelection gaFitness) seedPop
  print resultPop

--[[2,1,0,87,1,0.7706849894733506,1,0.9827379060664344,1,1,1290238869 84738397,1381],[2,1,0,242,1,0.8269040409161439,1,0.9827379060664344,1,1,1290238869 84738397,2395],[2,1,0,31,1,0.7706849894733506,1,0.9827379060664344,1,1,687239655 66155056,1381],[2,1,0,87,1,0.7706849894733506,1,0.9827379060664344,1,1,1290238869 84738397,1381],[2,0,0,97,0,0.7194222604509124,1,0.3787550245023832,0,1,611369370 1474892912,774]]
--142.696475326s