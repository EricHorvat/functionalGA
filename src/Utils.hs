module Utils (
  rev
)where

rev :: [a] -> [a]
rev = foldl (flip (:)) []

boundedInt :: Double -> (Int,Int) -> Int
boundedInt v (min,max) = min + floor (v * fromIntegral (max - min))