module Character (
warriorFitness,
archerFitness,
keeperFitness,
assassinFitness,
characterChromosomeGenerator
) where

import GABase
import Mutation

getItemIndexValue :: Int -> Allele -> Double
getItemIndexValue itemIndex (Vestment values index) = (values!!index)!!itemIndex

strengthItem = getItemIndexValue 0

agilityItem = getItemIndexValue 1

expertiseItem = getItemIndexValue 2

resistanceItem = getItemIndexValue 3

hpItem = getItemIndexValue 4

getAttribute :: Double -> (Allele -> Double) -> FitnessFunction
getAttribute coef itemFunction alleles = coef * tanh (0.01 * sum (map itemFunction alleles))

agility :: FitnessFunction
agility = getAttribute 1.0 agilityItem

expertise :: FitnessFunction
expertise = getAttribute 0.6 expertiseItem

strength :: FitnessFunction
strength = getAttribute 100.0 strengthItem

resistance :: FitnessFunction
resistance = getAttribute 100.0 resistanceItem

hp :: FitnessFunction
hp = getAttribute 100.0 hpItem

atm :: Double -> Double
atm h = 0.5 - (3 * h - 5) ** 4 + (3 * h - 5) ** 2 + h / 2

dem :: Double -> Double
dem h = 2   + (3 * h - 5) ** 4 - (3 * h - 5) ** 2 - h / 2

attack :: FitnessFunction
attack alleles = (agility vestments + expertise vestments) *
  strength vestments *
  atm height where
    vestments = tail alleles
    height = alleleValue (head alleles)

defense :: FitnessFunction
defense alleles = (resistance vestments + expertise vestments) *
  hp vestments *
  dem height where
    vestments = tail alleles
    height = alleleValue (head alleles)

characterFitness :: Double -> Double -> FitnessFunction
characterFitness aCoef dCoef alleles =
  aCoef * attack alleles +
  dCoef * defense alleles

warriorFitness :: FitnessFunction
warriorFitness = characterFitness 0.6 0.4

archerFitness :: FitnessFunction
archerFitness = characterFitness 0.9 0.1

keeperFitness :: FitnessFunction
keeperFitness = characterFitness 0.1 0.9

assassinFitness :: FitnessFunction
assassinFitness = characterFitness 0.7 0.3

characterChromosomeGenerator :: [[[Double]]] -> ChromosomeGenerator
characterChromosomeGenerator values seed = mutateFully seed ( BoundedDouble (1.3,2.0) 0.0 : map (flip Vestment 0) values)